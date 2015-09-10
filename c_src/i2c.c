/* i2c.c --- 
 * 
 * 
 */

/* Commentary:
 *
 * I2C driver for erlang/uclinux on the Blackfin. Some code and ideas
 * borrowed from Tony Rogvall's I2C driver:
 * https://github.com/tonyrog/i2c
 *
 * This driver was written as an asynchronous driver using
 * driver_async() to safeguard against possible VM hangs due to some
 * delay while talking to I2C devices (which could be caused by noise,
 * contention, device failures, etc.).
 */

/* Change Log:
 * 
 * 
 */

/* Code: */

#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <unistd.h>
#include <errno.h>
#include <signal.h>
#include <sys/ioctl.h>
#include <linux/i2c.h>
#include <linux/i2c-dev.h>
#include <linux/types.h>

#include <erl_driver.h>

/* defines */
#ifdef __DEBUG
#define DEBUG(x, ...) printf("DEBUG %s:" x, __PRETTY_FUNCTION__, ##__VA_ARGS__)
#else
#define DEBUG(x, ...)
#endif

#define ERROR(x, ...) printf("ERROR %s:" x, __PRETTY_FUNCTION__, ##__VA_ARGS__)


#define CMD_OPEN           1
#define CMD_CLOSE          2
// #define CMD_SET_RETRIES 3
// #define CMD_SET_TIMEOUT 4
// #define CMD_SET_SLAVE   5
// #define CMD_SET_SLAVEF  6
// #define CMD_SET_TENBIT  7
// #define CMD_SET_PEC     8
// #define CMD_GET_FUNCS   9
// #define CMD_RDWR 10
#define CMD_SMBUS          11
// #define CMD_DEBUG       12

/* #define I2C_SMBUS_QUICK, 0). */
/* #define I2C_SMBUS_BYTE,	 1). */
/* #define I2C_SMBUS_BYTE_DATA, 2). */
/* #define I2C_SMBUS_WORD_DATA, 3). */
/* #define I2C_SMBUS_PROC_CALL, 4). */
/* #define I2C_SMBUS_BLOCK_DATA, 5). */
/* #define I2C_SMBUS_I2C_BLOCK_BROKEN, 6). */
/* #define(I2C_SMBUS_BLOCK_PROC_CALL, 7).	%% SMBus 2.0 */
/* #define(I2C_SMBUS_I2C_BLOCK_DATA, 8). */


#define STATE_CLOSED -1
#define STATE_IDLE    0
#define STATE_BUSY    1

typedef struct {
	ErlDrvPort port;
	int fd;
	uint16_t addr;
	int state;
} i2c_ctx_t;

/* async handler data */
typedef struct {
	i2c_ctx_t *ctx;
	uint16_t addr;
	union i2c_smbus_data data;
	struct i2c_smbus_ioctl_data args;
	int retval;
} i2c_async_data_t;	

/* prototypes */
static ErlDrvData i2c_start(ErlDrvPort port, char *buff);
static void i2c_stop(ErlDrvData handle);
void i2c_async(void *async_data);
static void i2c_ready_async(ErlDrvData handle, ErlDrvThreadData async_data);

/* globals */

/* functions */

/* marshalling routines */
#define ALIGNBYTES_SHORT (sizeof(short) - 1)
#define ALIGN_SHORT(val)                                    \
  (((unsigned)val + ALIGNBYTES_SHORT) & ~ALIGNBYTES_SHORT)

#define ALIGNBYTES_LONG (sizeof(long) - 1)
#define ALIGN_LONG(val)                                     \
  (((unsigned)val + ALIGNBYTES_LONG) & ~ALIGNBYTES_LONG)

#define ALIGNBYTES_INT (sizeof(int) - 1)
#define ALIGN_INT(val)                                  \
  (((unsigned)val + ALIGNBYTES_INT) & ~ALIGNBYTES_INT)

#define ALIGNBYTES_LONG_LONG (sizeof(long long) - 1)
#define ALIGN_LONG_LONG(val)                                        \
  (((unsigned)val + ALIGNBYTES_LONG_LONG) & ~ALIGNBYTES_LONG_LONG)

#define ALIGNBYTES_FLOAT (sizeof(float) - 1)
#define ALIGN_FLOAT(val)                                    \
  (((unsigned)val + ALIGNBYTES_FLOAT) & ~ALIGNBYTES_FLOAT)

#define ALIGNBYTES_DOUBLE (sizeof(double) - 1)
#define ALIGN_DOUBLE(val)                                       \
  (((unsigned)val + ALIGNBYTES_DOUBLE) & ~ALIGNBYTES_DOUBLE)

#define ALIGNBYTES_LONG_DOUBLE (sizeof(long double) - 1)
#define ALIGN_LONG_DOUBLE(val)                                          \
  (((unsigned)val + ALIGNBYTES_LONG_DOUBLE) & ~ALIGNBYTES_LONG_DOUBLE)

/* unmarshal short */
int get_short(short *value, uint8_t *buffer, int bufidx)
{
	bufidx = ALIGN_SHORT(bufidx);
	short *pShort = (short *)&buffer[bufidx];
	*value = *pShort;
	return bufidx += sizeof(short);
}

/* unmarshal unsigned Short */
int get_ushort(unsigned short *value, uint8_t *buffer, int bufidx)
{
	bufidx = ALIGN_SHORT(bufidx);
	unsigned short *puShort = (unsigned short *)&buffer[bufidx];
	*value = *puShort;
	return bufidx += sizeof(unsigned short);
}

/* unmarshal signed int */
int get_int(int *value, uint8_t *buffer, int bufidx)
{
	bufidx = ALIGN_INT(bufidx);
	int *pint = (int *)&buffer[bufidx];
	*value = *pint;
	return bufidx += sizeof(int);
}

/* unmarshal unsigned int */
int get_uint(unsigned int *value, uint8_t *buffer, int bufidx)
{
	bufidx = ALIGN_INT(bufidx);
	unsigned int *puint = (unsigned int *)&buffer[bufidx];
	*value = *puint;
	return bufidx += sizeof(unsigned int);
}

/* unmarshal long */
int get_long(int *value, uint8_t *buffer, int bufidx)
{
	bufidx = ALIGN_INT(bufidx);
	int *pint = (int *)&buffer[bufidx];
	*value = *pint;
	return bufidx += sizeof(int);
}

/* unmarshal unsigned long */
int get_ulong(unsigned long *value, uint8_t *buffer, int bufidx)
{
	bufidx = ALIGN_INT(bufidx);
	unsigned long *puint = (unsigned long *)&buffer[bufidx];
	*value = *puint;
	return bufidx += sizeof(unsigned long);
}

/* unmarshal long long */
int get_longlong(long long *value, uint8_t *buffer, int bufidx)
{
	bufidx = ALIGN_LONG_LONG(bufidx);
	long long *plong = (long long *)&buffer[bufidx];
	*value = *plong;
	return bufidx += sizeof(long long);
}

/* unmarshal unsigned long long */
int get_ulonglong(unsigned long long *value, uint8_t *buffer, int bufidx)
{
	bufidx = ALIGN_LONG_LONG(bufidx);
	unsigned long long *pulong = (unsigned long long *)&buffer[bufidx];
	*value = *pulong;
	return bufidx += sizeof(unsigned long long);
}

/* unmarshal double */
int get_double(double *value, uint8_t *buffer, int bufidx)
{
	bufidx = ALIGN_DOUBLE(bufidx);
	double *pdouble = (double *)&buffer[bufidx];
	*value = *pdouble;
	return bufidx += sizeof(double);
}

/* unmarshal long double */
int get_longdouble(long double *value, uint8_t *buffer, int bufidx)
{
	bufidx = ALIGN_LONG_DOUBLE(bufidx);
	long double *pdouble = (long double *)&buffer[bufidx];
	*value = *pdouble;
	return bufidx += sizeof(long double);
}

/* unmarshal float */
int get_float(float *value, uint8_t *buffer, int bufidx)
{
	bufidx = ALIGN_FLOAT(bufidx);
	float *pfloat = (float *)&buffer[bufidx];
	*value = *pfloat;
	return bufidx += sizeof(float);
}

/* unmarshal octet */
int get_octet(uint8_t *value, uint8_t *buffer, int bufidx)
{
	uint8_t *poctet = (uint8_t *)&buffer[bufidx];
	*value = *poctet;
	return bufidx+1;
}

/* unmarshal char */
int get_char(char *value, uint8_t *buffer, int bufidx)
{
	char *pchar = (char *)&buffer[bufidx];
	*value = *pchar;
	return bufidx+1;
}

static ErlDrvData i2c_start(ErlDrvPort port, char *buff)
{
	i2c_ctx_t* ctx = (i2c_ctx_t *)driver_alloc(sizeof(i2c_ctx_t));

	if (ctx == NULL) {
		return NULL;
	}

	ctx->port = port;
	ctx->fd = -1;
	ctx->addr = 0xffff; 	/* initially invalid */
	ctx->state = STATE_CLOSED;

	DEBUG("i2c driver starting...\n");

	/* set port IO to binary */
	set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);

	return (ErlDrvData)ctx;
}

static void i2c_stop(ErlDrvData handle)
{
	i2c_ctx_t *ctx = (i2c_ctx_t *)handle;
	if (ctx->fd > 0) {
		close(ctx->fd);
		ctx->state = STATE_CLOSED;
	}
	driver_free((char*)handle);
}

void i2c_async(void *async_data)
{
	i2c_async_data_t *data = (i2c_async_data_t *)async_data;
	int ret;

	/* no error yet */
	data->retval = 0;

	/* check if we need to set a new slave address */
	if (data->addr != data->ctx->addr) {
		/* set new addr */
		ret = ioctl(data->ctx->fd, I2C_SLAVE, data->addr);
		if (ret < 0) {
			ERROR("set slave addr failed: %s\n", strerror(errno));
			data->retval = errno;
			return;
		}
		data->ctx->addr = data->addr;
	}

	/* execute the command */
	if (ioctl(data->ctx->fd, I2C_SMBUS, &data->args) < 0) {
		ERROR("ioctl failed: %s\n", strerror(errno));
		data->retval = errno;
	}

	/* if we make it here, the command succeeded, and the returned
	 * data, if any, is in the i2c structs */
}

static void i2c_ready_async(ErlDrvData handle, ErlDrvThreadData async_data)
{
    i2c_ctx_t* ctx = (i2c_ctx_t *)handle;
    i2c_async_data_t *data = (i2c_async_data_t *)async_data;
    char hbuf;
    int size = 0;

    /* transaction complete */
    ctx->state = STATE_IDLE;
    
    if (data->retval) {
	    char* err_str = erl_errno_id(data->retval);
	    hbuf = 255;
	    driver_output2(ctx->port, &hbuf, 1,  err_str, strlen(err_str));
	    return;
    }

    /* return result to caller */
    switch(data->args.size) {
    case I2C_SMBUS_QUICK:
	    hbuf = 0;
	    size = 0;
    	    break;
    case I2C_SMBUS_BYTE:
	    hbuf = 1;
	    size = 1;
    	    break;
    case I2C_SMBUS_BYTE_DATA:
	    hbuf = 1;
	    size = 1;
    	    break;
    case I2C_SMBUS_WORD_DATA:
	    hbuf = 2;
	    size = 2;
    	    break;
    case I2C_SMBUS_PROC_CALL:
	    hbuf = 3;
	    size = sizeof(data->data.block);
    	    break;
    case I2C_SMBUS_BLOCK_DATA:
	    hbuf = 3; 	    
	    size = sizeof(data->data.block);
    	    break;
    /* case I2C_SMBUS_I2C_BLOCK_BROKEN: */
    /* 	    break; */
    /* case I2C_SMBUS_BLOCK_PROC_CALL: */
    /* 	    break; */
    /* case I2C_SMBUS_I2C_BLOCK_DATA: */
    /* 	    break; */
    }

    driver_output2(ctx->port, &hbuf, 1, (char *)data->data.block, size);
    driver_free((char *)data);
}

/* general control reply function */
static ErlDrvSSizeT ctl_reply(int rep, void* buf, ErlDrvSizeT len,
			      char** rbuf, ErlDrvSizeT rsize)
{
	char* ptr;
	if ((len+1) > rsize) {
		/* this will get freed automatically when control returns */
		ErlDrvBinary* bin = driver_alloc_binary(len+1);
		if (bin == NULL)
			return -1;
		ptr = bin->orig_bytes;	
		*rbuf = (char*) bin;
	} else {
		ptr = *rbuf;
	}
	*ptr++ = rep;
	if (buf != NULL) {
		memcpy(ptr, buf, len);
	}
	return len+1;
}

static ErlDrvSSizeT i2c_control(ErlDrvData handle, 
				unsigned int cmd, 
				char* buf0, 
				ErlDrvSizeT len, 
				char** rbuf, 
				ErlDrvSizeT rsize)
{
	i2c_ctx_t* ctx = (i2c_ctx_t*)handle;
	i2c_async_data_t *async_data;
	int bufidx;
	uint8_t *buf = (uint8_t *)buf0;

	switch(cmd) {
	case CMD_OPEN:
		if (ctx->state != STATE_CLOSED) {
			goto error;
		}

		/* open the i2c device */
		ctx->fd = open("/dev/i2c-0", O_RDWR);
		DEBUG("opened i2c device\n");
		if (ctx->fd < 0) {
			ERROR("can't open i2c device %s\n", strerror(errno));
			goto error;
		}
		ctx->state = STATE_IDLE;
		goto ok;
	case CMD_CLOSE:
		if (ctx->state != STATE_IDLE) {
			goto error;
		}
		close(ctx->fd);
		ctx->state = STATE_CLOSED;
		DEBUG("closed i2c device\n");
		goto ok;
	/* case CMD_RDWR: */
	case CMD_SMBUS: // <<Addr:16,ReadWrite:8,Command:8,Size:32,Data/binary>>
		if (ctx->state != STATE_IDLE) {
			goto error;
		}

		async_data = driver_alloc(sizeof(i2c_async_data_t));
		if (async_data == NULL) {
			goto error;
		}

		/* check for bad arguments */
		if (len < 6)  {
			goto badarg;
		}

		bufidx = 0;
		bufidx = get_ushort(&async_data->addr, buf, bufidx);
		bufidx = get_octet(&async_data->args.read_write, buf, bufidx);
		bufidx = get_octet(&async_data->args.command, buf, bufidx);
		bufidx = get_uint(&async_data->args.size, buf, bufidx);
		
		DEBUG("CMD_SMBUS: addr = 0x%x, rw=%d, cmd=%d, size=%d\n", 
		       async_data->addr,
		       async_data->args.read_write,
		       async_data->args.command,
		       async_data->args.size);

		async_data->args.data = &async_data->data;
		len -= bufidx;
		memset(async_data->data.block, 0, sizeof(async_data->data));
		if (len > sizeof(async_data->data))
			memcpy(async_data->data.block, &buf[bufidx], sizeof(async_data->data));
		else if (len > 0)
			memcpy(async_data->data.block, &buf[bufidx], len);

		async_data->ctx = ctx;

		ctx->state = STATE_BUSY;

		/* done with work prep, schedule the async handler */
		driver_async(ctx->port, NULL, i2c_async, async_data, NULL);
		goto pending;
	default:
		goto error;
	}

pending:
	return ctl_reply(1, NULL, 0, rbuf, rsize);
ok:
	return ctl_reply(0, NULL, 0, rbuf, rsize);
badarg:
	errno = EINVAL;
	goto error;
error:
	{
		char* err_str = erl_errno_id(errno);
		return ctl_reply(255, err_str, strlen(err_str), rbuf, rsize);
	}
}


ErlDrvEntry i2c_entry = {
    NULL,	        /* F_PTR init, called when driver is loaded */
    i2c_start,          /* L_PTR start, called when port is opened */
    i2c_stop,           /* F_PTR stop, called when port is closed */
    NULL,               /* F_PTR output, called when erlang has sent us a message*/
    NULL,               /* F_PTR ready_input, called when input descriptor ready */
    NULL,               /* F_PTR ready_output, called when output descriptor ready */
    "i2c",	        /* char *driver_name, the argument to open_port */
    NULL,	        /* F_PTR finish, called when unloaded */
    NULL,               /* void *handle, Reserved by VM */
    i2c_control,	/* F_PTR control, port_command callback */
    NULL,	        /* F_PTR timeout, reserved */
    NULL,		/* F_PTR outputv, reserved */
    i2c_ready_async,    /* F_PTR ready_async, only for async drivers */
    NULL,               /* F_PTR flush, called when port is about to
				   be closed, but there is data in
				   driver queue */
    NULL,               /* F_PTR call, much like control, sync call to
				   driver */
    NULL,               /* F_PTR event, called when an event selected
				   by driver_event() occurs. */
    ERL_DRV_EXTENDED_MARKER,    /* int extended marker, Should always be 
				   set to indicate driver versioning */
    ERL_DRV_EXTENDED_MAJOR_VERSION, /* int major_version, should always be 
				       set to this value */
    ERL_DRV_EXTENDED_MINOR_VERSION, /* int minor_version, should always be 
				       set to this value */
    0,                  /* int driver_flags, see documentation */
    NULL,               /* void *handle2, reserved for VM use */
    NULL,               /* F_PTR process_exit, called when a monitored
			   process dies */
    NULL                /* F_PTR stop_select, called to close an event
				   object */
};

DRIVER_INIT(i2c) /* must match name in driver_entry */
{
    return &i2c_entry;
}
	
/* i2c.c ends here */
