/* i2c_port.c --- 
 * 
 * Filename: i2c_port.c
 * Description: 
 * Author: Devin Butterfield
 * Maintainer: 
 * Created: Sat Nov  2 11:48:15 2013 (-0700)
 * Version: 
 * Last-Updated: Thu Sep 11 11:47:23 2014 (-0700)
 *	     By: Devin Butterfield
 *     Update #: 779
 * URL: 
 * Keywords: 
 * Compatibility: 
 * 
 */

/* Commentary: 
 * 
 */

/* Change Log:
 * 
 * 
 */

/* *This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 3, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the GNU
 * General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth
 * Floor, Boston, MA 02110-1301, USA.
 */

/* Code: */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <unistd.h>
#include <errno.h>
#include <signal.h>
#include <sys/ioctl.h>
#include <linux/i2c-dev.h>

/* for logging */
#include <syslog.h>

/* defines */

/* operations (commands) we support */
#define OPR_READ	    1	/* read from i2c device */
#define OPR_READ_REG	    2	/* read from i2c device register */
#define OPR_WRITE	    3	/* write to i2c device */
#define OPR_WRITE_REG	    4	/* write to i2c device register */

/* response codes */
#define RES_OK	   0		/* command OK */
#define RES_DATA   1		/* command OK, data returned */
#define RES_ERR	   255		/* command failed */

/* prototypes */
int read_cmd(unsigned char *buf);
int read_exact(unsigned char *buf, int len);
int write_cmd(unsigned char *buf, int len);
int write_exact(unsigned char *buf, int len);
int i2c_read_write(uint16_t addr,
		      uint16_t reg,
		      uint16_t size,
		      uint8_t *buf,
		      int read_write);

/* globals */

/* i2c driver file descriptor */
/* static int fd; */

/* functions */

/*
 * The following routines taken from the erlang docs
 */
int read_cmd(unsigned char *buf)
{
	int len;

	if (read_exact(buf, 2) != 2)
		return -1;
	len = (int)((buf[0] << 8) | buf[1]);

	return read_exact(buf, len);
}

int read_exact(unsigned char *buf, int len)
{
	int i, got=0;
  
	do {
		if ((i = read(0, buf+got, len-got)) <= 0) {
			syslog(LOG_ERR, "error: read returned %d\n", i);
			return(i);
		}
    
		got += i;
	} while (got<len);

	return(len);
}

int write_cmd(unsigned char *buf, int len)
{
	unsigned char b;

	b = (len >> 8) & 0xff;
	write_exact(&b, 1);

	b = len & 0xff;
	write_exact(&b, 1);

	return write_exact(buf, len);
}

int write_exact(unsigned char *buf, int len)
{
	int i, wrote = 0;
	do {
		if ((i = write(1, buf+wrote, len-wrote)) <= 0)
			return (i);
		wrote += i;
	} while (wrote<len);
  
	return (len);
}

/*
 * Functions:
 *
 * 1. read device i2c register
 *   - args: addr, reg, size, return size bytes or error resp code
 * 2. write device i2c register
 *   - args: addr, reg, size, data, return resp code
 */
 
    
/* The protocol for communications with the port is simple. Messages
 * have the following structure:
 *
 * [uint16_t msg len][uint16_t operation/result type][args...]
 * max length is 50 bytes
 */ 

int main(int argc, char **argv)
{
	int i, len, res_data_len;
	uint16_t op,res;
	uint16_t *cr;
	uint16_t *input;
	uint16_t *soutput;
	uint8_t *boutput;
	unsigned char ibuf[48],obuf[48]; /* 2 len + 2 opcode + args */
	uint16_t chan;		/* pub/sub channel number */
	uint16_t addr;		/* i2c slave address */
	uint16_t reg;			/* i2c device register */
	uint16_t size;		/* data size (in bytes) */
	uint32_t period;		/* publisher period (usecs) */
	uint32_t task_prio;		/* pub/sub task priority */
	uint32_t q_policy;		/* message queuing policy */
	uint32_t max_msgs;		/* max messages in queue */
	uint32_t topic_name_len;	/* topic name length */
	char *topic_name;		/* topic name */
	int32_t val;

	/* union i2c_smbus_data data; */
  
        /* open the log */
	openlog("i2c_port", 0, LOG_USER);
	syslog(LOG_INFO, "i2c port starting...\n");

	/* set this higher to see INFO and DEBUG messages */
	/* TODO: make this a run-time configurable setting */
	setlogmask(LOG_UPTO(LOG_ERR));

	/* initialization */
	input = (uint16_t *)&ibuf[2];
	soutput = (uint16_t *)&obuf[2];
	boutput = &obuf[2];
  
        /* open the device */
	fd = open("/dev/i2c-0", O_RDWR);
	if (device < 0) {
		syslog(LOG_ERR, "ERROR : can't open i2c device %s\n", strerror(-fd));
		exit(1);
	}

	/* handle any args here */

	syslog(LOG_INFO, "going to wait for input...\n");
	while ((len = read_cmd(ibuf)) > 0) {
		/* handle command */
		cr = (uint16_t *)&ibuf[0];
		op = *cr;

		syslog(LOG_DEBUG, "got command, op=0x%x, len=%d\n\n",
			op,len);
		i = 0;
		res_data_len = 0;
		switch (op) {
		case OPR_READ:
			addr = input[0];
			size = input[1];
			syslog(LOG_DEBUG, "got addr 0x%.2x\n", addr);
			syslog(LOG_DEBUG, "got size %d\n", size);

			/* set addr */
			res = ioctl(fd, I2C_SLAVE, addr);
			if (res < 0) {
				syslog(LOG_ERR, "set slave addr failed: %s\n", strerror(errno));
				close(fd);
				return -1;
			}

			/* do the read */
			switch(size) {
			case 1:
				res = i2c_smbus_read_byte(fd);
				if (res < 0) {
					syslog(LOG_ERR, "ERROR : can't read from device %s\n",
					       strerror(-device));
					close(fd);
					return -1;
				}
				*boutput = (uint8_t)res&0xff;
				break;
			case 2:
				res = i2c_smbus_read_word(fd);
				if (res < 0) {
					syslog(LOG_ERR, "ERROR : can't read from device %s\n",
					       strerror(-device));
					close(fd);
					return -1;
				}
				*soutput = (uint16_t)res&0xffff;
				
				break;
			default:
				syslog(LOG_ERR, "invalid size %d for i2c transfer\n", size);
				close(fd);
				return -1;
			}

			res = RES_DATA;
			res_data_len = size;
			break;
		case OPR_READ_REG:
			addr = input[0];
			reg = input[1];
			size = input[2];
			syslog(LOG_DEBUG, "got addr 0x%.2x\n", addr);
			syslog(LOG_DEBUG, "got reg 0x%.2x\n", reg);
			syslog(LOG_DEBUG, "got size %d\n", size);

			/* set addr */
			ret = ioctl(fd, I2C_SLAVE, addr);
			if (ret < 0) {
				syslog(LOG_ERR, "set slave addr failed: %s\n", strerror(errno));
				close(fd);
				return -1;
			}

			/* do the read */
			switch(size) {
			case 1:
				res = i2c_smbus_read_byte_data(fd, reg);
				if (res < 0) {
					syslog(LOG_ERR, "ERROR : can't read from device %s\n",
					       strerror(-device));
					close(fd);
					return -1;
				}
				*boutput = (uint8_t)res&0xff;
				break;
			case 2:
				res = i2c_smbus_read_word_data(fd, reg);
				if (res < 0) {
					syslog(LOG_ERR, "ERROR : can't read from device %s\n",
					       strerror(-device));
					close(fd);
					return -1;
				}
				*soutput = (uint16_t)res&0xffff;
				break;
			default:
				syslog(LOG_ERR, "invalid size %d for i2c transfer\n", size);
				close(fd);
				return -1;
			}
			res = RES_DATA;
			res_data_len = size;
			break;
		case OPR_WRITE:
			addr = input[0];
			size = input[1];
			syslog(LOG_DEBUG, "got addr 0x%.2x\n", addr);
			syslog(LOG_DEBUG, "got size %d\n", size);

			/* set addr */
			ret = ioctl(fd, I2C_SLAVE, addr);
			if (ret < 0) {
				syslog(LOG_ERR, "set slave addr failed: %s\n", strerror(errno));
				close(fd);
				return -1;
			}

			/* do the write */
			switch(size) {
			case 1:
				res = i2c_smbus_write_byte(fd, *(uint8_t *)&input[3]);
				if (res < 0) {
					syslog(LOG_ERR, "ERROR : can't write to device %s\n",
					       strerror(-fd));
					close(fd);
					return -1;
				}
				break;
			case 2:
				res = i2c_smbus_write_word(fd, *(uint16_t *)&input[3]);
				if (res < 0) {
					syslog(LOG_ERR, "ERROR : can't write to device %s\n",
					       strerror(-fd));
					close(fd);
					return -1;
				}
				break;
			default:
				syslog(LOG_ERR, "invalid size %d for i2c transfer\n", size);
				close(fd);
				return -1;
			}
 			res = RES_OK;
			res_data_len = 0;
			break;
		case OPR_WRITE_REG:
			addr = input[0];
			reg = input[1];
			size = input[2];
			syslog(LOG_DEBUG, "got addr 0x%.2x\n", addr);
			syslog(LOG_DEBUG, "got reg 0x%.2x\n", reg);
			syslog(LOG_DEBUG, "got size %d\n", size);
           
			/* set addr */
			ret = ioctl(fd, I2C_SLAVE, addr);
			if (ret < 0) {
				syslog(LOG_ERR, "set slave addr failed: %s\n", strerror(errno));
				close(fd);
				return -1;
			}

			/* do the write */
			switch(size) {
			case 1:
				res = i2c_smbus_write_byte_data(fd, reg, *(uint8_t *)&input[3]);
				if (res < 0) {
					syslog(LOG_ERR, "ERROR : can't write to device %s\n",
					       strerror(-fd));
					close(fd);
					return -1;
				}
				break;
			case 2:
				res = i2c_smbus_write_word_data(fd, reg, *(uint16_t *)&input[3]);
				if (res < 0) {
					syslog(LOG_ERR, "ERROR : can't write to device %s\n",
					       strerror(-fd));
					close(fd);
					return -1;
				}
				break;
			default:
				syslog(LOG_ERR, "invalid size %d for i2c transfer\n", size);
				close(fd);
				return -1;
			}
 			res = RES_OK;
			res_data_len = 0;
			break;
		default:
			syslog(LOG_ERR, "error: invalid command %x\n", op);
			res = RES_ERR;
		}

		syslog(LOG_DEBUG, "sending result, len=%d, res=%x\n", len, res);

		/* handle result */
		cr = (uint16_t *)&obuf[0];
		*cr = res;
		write_cmd(obuf, 2+res_data_len);
	}
  
	close(fd);
  
	syslog(LOG_INFO, "i2c_port exiting...\n");

	return 0;
}
	
/* i2c_port.c ends here */
