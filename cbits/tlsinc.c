
#define _BSD_SOURCE       1
#define _XOPEN_SOURCE     1   /* or any value < 500 */
#define _POSIX_C_SOURCE     1



#include <sys/types.h>
#include <signal.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <pthread.h>
#include <bits/sigthread.h>
#include <sys/time.h>
#include <sys/types.h>

#include <openssl/rand.h>
#include <openssl/ssl.h>
#include <openssl/err.h>

// Simple structure to keep track of the handle, and
// of what needs to be freed later.
typedef struct {
    int socket;
    SSL_CTX *sslContext;
    char* protocol_list;
    int protocol_list_length;
} connection_t;

// Simple structure representing a session here
typedef struct {
    int socket;
    SSL *sslHandle;
    // Protocol index selected during negotiation....
    int protocol_index;
} wired_session_t;

// This is the public API of this server...
// No arguments, all are wired here somewhere... for now
connection_t* make_connection();
// Call when you are done 
void close_connection(connection_t* conn);
// Wait for the next one...
#define ALL_OK  0 
#define BAD_HAPPENED 1
#define TIMEOUT_REACHED 3
// This is also a failed IO with SSL, but this one may be quite
// natural and we want to handle it differently
#define TRANSPORT_CLOSED 2
int wait_for_connection(connection_t* conn, int microseconds, wired_session_t** wired_session);
int send_data(wired_session_t* ws, char* buffer, int buffer_size);
int recv_data(wired_session_t* ws, char* inbuffer, int buffer_size, int* data_recvd);
int get_selected_protocol(wired_session_t* ws){ return ws->protocol_index; }
void dispose_wired_session(wired_session_t* ws);
static int thread_setup(void);


////////////////////////////////////////////////////////////////////////


static int threads_are_up = 0;

// Leaky implementation now 
void close_connection(connection_t* conn)
{
    if (conn->socket)
    {
        close (conn->socket);
        conn->socket = 0;
    }

    if (conn->sslContext)
    {
        SSL_CTX_free(conn->sslContext);
        conn->sslContext = 0;
    }

    free(conn);
}

// For this example, we'll be testing on openssl.org


// Establish a regular tcp connection
static int tcpStart (char* hostname, int portno, int* errorh)
{
    int error, handle;
    *errorh = 0;
    struct hostent *host;
    struct sockaddr_in server;

    bzero((char *) &server, sizeof(server));


    host = gethostbyname (hostname);
    handle = socket (AF_INET, SOCK_STREAM, 0);
    int one = 1;
    setsockopt(handle, SOL_SOCKET, SO_REUSEADDR, &one, sizeof(one));
    if (handle == -1)
    {
        perror ("Socket could not be created");
        handle = 0;
        *errorh = BAD_HAPPENED;
    }
    else
    {
        server.sin_family = AF_INET;
        server.sin_port = htons (portno);
        server.sin_addr = *((struct in_addr *) host->h_addr);
        bzero (&(server.sin_zero), 8);

        error = bind (handle, (struct sockaddr *) &server,
                sizeof (struct sockaddr));
        if (error == -1)
        {
            perror ("Bind");
            handle = 0;
            *errorh = BAD_HAPPENED;
        }
        else 
        {
            error = listen(handle, 5);
            if ( error != 0 )
            {
                perror("Listen");
                handle = 0;
                *errorh = BAD_HAPPENED;
            }
        }
    }

    return handle;
}

// Dh_Callback {{{
#ifndef HEADER_DH_H
#include <openssl/dh.h>
#endif
DH *get_dh2236()
{
    static unsigned char dh2236_p[]={
        0x08,0x32,0x3B,0x6A,0xE8,0xEA,0x55,0xE3,0x0C,0xD9,0x95,0x3D,
        0xE7,0x78,0xDA,0xF0,0x05,0x81,0x94,0x85,0x34,0x5C,0x26,0x5A,
        0xAE,0x9E,0x31,0x8F,0x0C,0xCC,0xFC,0xF3,0x30,0x88,0x20,0x0B,
        0x41,0xC3,0xC9,0xEC,0x13,0xA7,0xFF,0xDA,0xBB,0x11,0xFB,0x7B,
        0x19,0x92,0x3B,0x61,0xDF,0x31,0xEF,0x2C,0x4A,0x5C,0xD1,0x59,
        0x16,0x90,0xAC,0xFC,0xEA,0xFF,0xCB,0xC4,0x04,0x07,0x93,0x9D,
        0xF0,0x70,0x0B,0xCB,0x3E,0x79,0x0D,0xEB,0xD3,0x3E,0x06,0x0B,
        0x19,0xF2,0x97,0x1E,0xE1,0xBF,0xD2,0xBE,0x5E,0xA5,0xBD,0x5F,
        0x73,0x2C,0x57,0x0C,0xB0,0x97,0x78,0xAA,0x60,0xDC,0x98,0x89,
        0xAF,0xBF,0xCD,0x49,0x74,0x82,0x64,0x09,0x60,0x47,0xD0,0xD4,
        0x5D,0x25,0xDC,0x38,0xAC,0x17,0xE7,0xA4,0x47,0x59,0x94,0xFE,
        0xA8,0xAD,0x58,0xD0,0xD1,0x43,0x3C,0x20,0x2E,0x34,0xEE,0xA9,
        0x0F,0x71,0x20,0x93,0xB7,0x8E,0xEC,0xB1,0x75,0xB9,0xE9,0x9A,
        0xAB,0x73,0xBB,0x7F,0xA2,0x8F,0x11,0xDE,0x58,0x5E,0xB0,0x98,
        0xD6,0x95,0x84,0x62,0x90,0x87,0x90,0x32,0x34,0xF7,0x61,0x28,
        0xFF,0x17,0xD0,0x58,0x33,0xA6,0xC2,0xC6,0x58,0x65,0x1C,0x92,
        0xF3,0xDF,0x4D,0xB3,0xB0,0xFD,0xD2,0x4A,0x97,0x1B,0xA7,0xD2,
        0x7C,0x8D,0x8F,0x1F,0x69,0x98,0x54,0xD9,0x33,0x15,0xE5,0xEA,
        0xBA,0xAA,0x31,0xB1,0x17,0x65,0x21,0xFA,0xC7,0x54,0xC0,0xE4,
        0x72,0x3C,0x15,0x74,0x41,0xF2,0x9A,0xF6,0xD1,0x16,0x10,0x35,
        0x4F,0x36,0xA6,0x23,0x67,0x67,0x89,0x65,0xDD,0x77,0x5D,0x8F,
        0x69,0x49,0x22,0x24,0xB6,0xB8,0x45,0xCA,0x8F,0x35,0x71,0x45,
        0x4C,0x65,0x38,0x11,0x21,0xD5,0x39,0x03,0x4D,0x05,0xBB,0x95,
        0x43,0x58,0xBB,0xF3,
    };
    static unsigned char dh2236_g[]={
        0x02,
    };
    DH *dh;

    if ((dh=DH_new()) == NULL) return(NULL);
    dh->p=BN_bin2bn(dh2236_p,sizeof(dh2236_p),NULL);
    dh->g=BN_bin2bn(dh2236_g,sizeof(dh2236_g),NULL);
    if ((dh->p == NULL) || (dh->g == NULL))
    { DH_free(dh); return(NULL); }
    return(dh);
}
DH *get_dh2048()
{
    static unsigned char dh2048_p[]={
        0xCE,0x28,0x14,0x1C,0xEF,0x22,0x1D,0x86,0xEA,0x10,0x00,0x50,
        0x24,0x42,0x95,0xC3,0x07,0x5A,0x87,0xED,0x0F,0xC5,0xDC,0x0F,
        0x5E,0x7E,0x69,0x25,0x85,0x90,0x39,0x60,0x1E,0x87,0x5C,0x4B,
        0xAD,0xDF,0xA8,0xF4,0x9C,0xC6,0x2D,0x6A,0x2E,0x7C,0xD1,0x5C,
        0xC5,0x1A,0x74,0xD3,0x9E,0xE1,0xBB,0x31,0xC4,0x23,0x2F,0x78,
        0xF5,0x61,0x5C,0x09,0x7E,0x29,0x06,0xC5,0x07,0x50,0x32,0x80,
        0x4A,0xBE,0x5F,0x7F,0x68,0x69,0x8F,0xB1,0x6D,0xA0,0x0C,0x7F,
        0x9E,0x28,0x77,0x9D,0x4F,0xEA,0xB5,0x38,0xB3,0x72,0x8E,0x1D,
        0x8D,0x1C,0x58,0x74,0x58,0xC4,0xDD,0x06,0x8E,0x80,0x91,0x36,
        0x2D,0x42,0x3D,0xF0,0x11,0xEC,0xDF,0x02,0xFD,0x84,0x54,0x32,
        0x90,0xFE,0x7C,0x74,0x3A,0x5F,0x87,0xBA,0xD3,0x21,0x0E,0xDD,
        0xF4,0xB3,0xFD,0xF9,0x89,0x8F,0x96,0x59,0x90,0x74,0xE8,0x45,
        0x5A,0x3A,0x7C,0x88,0x7F,0xD8,0x5F,0xD2,0x32,0x57,0x46,0x29,
        0x6B,0xA1,0x0A,0x05,0x1E,0xB6,0x49,0x8A,0x68,0xB4,0xEE,0x84,
        0x45,0xEF,0x56,0x7E,0x59,0x83,0x67,0x20,0x85,0x63,0x69,0x6B,
        0x39,0xCA,0x24,0x46,0x68,0x51,0x94,0x9E,0x3E,0xB4,0x69,0x1F,
        0x63,0x07,0x40,0x0E,0x84,0x8B,0x26,0x98,0xCE,0xE5,0x48,0x2C,
        0xD8,0xF9,0x6A,0x3F,0x20,0xFA,0xFA,0xDC,0x4E,0xFA,0xBD,0xC3,
        0x81,0x09,0x4D,0xCF,0x07,0xEF,0xE8,0x32,0x9B,0x63,0x32,0x5B,
        0x06,0x59,0x4F,0xE7,0x5B,0xD2,0xFC,0x25,0xC6,0x2C,0xA3,0xE8,
        0x05,0xE5,0x8D,0xC2,0x94,0x76,0x90,0x63,0x29,0xB4,0xEE,0x2D,
        0xD6,0x50,0x83,0x8B,
    };
    static unsigned char dh2048_g[]={
        0x02,
    };
    DH *dh;

    if ((dh=DH_new()) == NULL) return(NULL);
    dh->p=BN_bin2bn(dh2048_p,sizeof(dh2048_p),NULL);
    dh->g=BN_bin2bn(dh2048_g,sizeof(dh2048_g),NULL);
    if ((dh->p == NULL) || (dh->g == NULL))
    { DH_free(dh); return(NULL); }
    return(dh);
}

DH *get_dh1024()
{
    static unsigned char dh1024_p[]={
        0x87,0xF4,0xE5,0x5A,0x1E,0x7F,0x98,0x83,0xFD,0x23,0x6A,0xF3,
        0x8C,0xE8,0x2F,0x35,0x64,0x3D,0x13,0x34,0x5B,0x0A,0x52,0xEC,
        0x0B,0x3A,0xBF,0x92,0xE4,0x67,0x14,0x47,0x86,0x55,0x2C,0x83,
        0x1B,0xDD,0x0E,0xC8,0x2D,0x98,0x72,0x2A,0xB6,0x68,0xF1,0x32,
        0xD8,0xBD,0x6B,0x17,0x23,0x46,0x08,0xB6,0x19,0x58,0x01,0x30,
        0x32,0x68,0x60,0x78,0xD1,0x5B,0x5C,0x88,0x3F,0x20,0xD7,0xEB,
        0xE8,0xD5,0x80,0xB0,0x23,0x75,0xAE,0x97,0x26,0xBD,0xA5,0x11,
        0x7F,0x83,0x8C,0x21,0xD4,0x39,0x52,0xE0,0x25,0x1F,0x03,0xA9,
        0x69,0xF8,0x07,0x4F,0x33,0x17,0x72,0xCC,0xEB,0xBD,0x77,0xD1,
        0x9C,0x40,0xA6,0x55,0xE3,0x87,0x76,0x66,0x3B,0xE3,0xE4,0x6A,
        0x4B,0x6B,0xF1,0x92,0x1A,0x3A,0x25,0xC3,
    };
    static unsigned char dh1024_g[]={
        0x02,
    };
    DH *dh;

    if ((dh=DH_new()) == NULL) return(NULL);
    dh->p=BN_bin2bn(dh1024_p,sizeof(dh1024_p),NULL);
    dh->g=BN_bin2bn(dh1024_g,sizeof(dh1024_g),NULL);
    if ((dh->p == NULL) || (dh->g == NULL))
    { DH_free(dh); return(NULL); }
    return (dh);
}


static DH *tmp_dh_callback(SSL *s, int is_export, int keylength);


// Setup dh params 
static void setup_dh_parms(SSL_CTX *sslContext)
{
    /* Set up ephemeral DH stuff */
    SSL_CTX_set_tmp_dh_callback(sslContext, tmp_dh_callback);
}

static DH *tmp_dh_callback(SSL *s, int is_export, int keylength)
{
    static DH *dh_2048 = NULL;
    static DH *dh_1024 = NULL;
    DH *dh_tmp=NULL;
    switch (keylength) {
        case 2048:
            if (!dh_2048)
                dh_2048 = get_dh2048();
            dh_tmp = dh_2048;
            break;
        case 1024:
            if (!dh_1024)
                dh_1024 = get_dh1024();
            dh_tmp = dh_1024;
            break;
        default:
            /* Generating a key on the fly is very costly, so use what is there */
            printf("UNEXPECTED: Keylength %d \n", keylength);
    }
    return(dh_tmp);
}
// dh_callback }}}



static int protocol_select (
        SSL *ssl,
        const unsigned char **out,
        unsigned char *outlen,
        const unsigned char *in,
        unsigned int inlen,
        void *arg)
{
    // Oh well, C is a verbose beast
    connection_t* conn = (connection_t*) arg;
    static char output[64];

    char* incursor = (char*) in;

    while (incursor < (char*)in + inlen )
    {
        // Got a protocol.... can I satisfy it?
        char sublen = *incursor;
        //printf("offered prot %.*s \n", sublen, incursor+1);

        char* stored_cursor = conn->protocol_list;
        int sto_protocol = 0;

        while( stored_cursor < conn->protocol_list + conn->protocol_list_length)
        {
            char sublen2 = *stored_cursor;

            if (sublen != sublen2)
            {

            } else {
                int cmpresult = strncmp( incursor + 1, stored_cursor + 1, sublen);
                if (cmpresult == 0)
                {
                    // They are equal, choose this one...
                    strncpy( output, stored_cursor+1, sublen);
                    *outlen = sublen;
                    *out = output;


                    return SSL_TLSEXT_ERR_OK;
                }
            }
            sto_protocol += 1;
            stored_cursor += (1+sublen2);
        }

        incursor += (1+sublen);
    }

    // I think this is what should be returned 
    return -1;
}


int lookup_protocol(
        char* selected, int selected_len, 
        char* myprotocol_list, int mpl_len
        )
{
    char* incursor = selected;

    char* stored_cursor = myprotocol_list;
    int sto_protocol = 0;

    if (selected_len == 0)
    {
        // No protocol was selected
        return -2;
    }

    while( stored_cursor < myprotocol_list + mpl_len)
    {
        char sublen2 = *stored_cursor;

        if (sublen2 != selected_len)
        {
            // It is not this cone, continue
        } else {
            int cmpresult = strncmp( selected, stored_cursor + 1, sublen2);
            if (cmpresult == 0)
            {
                // They are equal, choose this one...
                return sto_protocol;
            }
        }
        sto_protocol += 1;
        stored_cursor += (1+sublen2);
    }

    // I think this is what should be returned 
    return -1;
}

static int ssl_servername_cb(SSL *s, int *ad, void *arg)
{
    // TODO: Use this for something .... although I'm not sure what...

    // tlsextctx *p = (tlsextctx *) arg;
    // const char *servername = SSL_get_servername(s, TLSEXT_NAMETYPE_host_name);
    // if (servername && p->biodebug)
    //     BIO_printf(p->biodebug, "Hostname in TLS extension: \"%s\"\n",
    //                servername);

    // if (!p->servername)
    //     return SSL_TLSEXT_ERR_NOACK;

    // if (servername) {
    //     if (strcasecmp(servername, p->servername))
    //         return p->extension_error;
    //     if (ctx2) {
    //         BIO_printf(p->biodebug, "Switching server context.\n");
    //         SSL_set_SSL_CTX(s, ctx2);
    //     }
    // }
    return SSL_TLSEXT_ERR_OK;
}


// Establish a connection using an SSL layer
static connection_t *sslStart (
        char* certificate_filename, char* privkey_filename, char* hostname, int portno,
        char* protocol_list, int protocol_list_length
        )
{
    int result;
    connection_t *c;

    // Ok, here it is in the open, for everybody 
    // to see:
    signal(SIGPIPE, SIG_IGN);

    if (! threads_are_up)
    {
        threads_are_up = 1;
        thread_setup();
    } 

    c = malloc (sizeof (connection_t));
    c->sslContext = NULL;
    c->protocol_list  = (char*) malloc(protocol_list_length);
    strncpy( c->protocol_list, protocol_list, protocol_list_length );
    c->protocol_list_length = protocol_list_length;
    c->socket = tcpStart (hostname, portno, &result);

    if ( result )
    {
        return 0;
    }

    if (c->socket)
    {
        // Register the error strings for libcrypto & libssl
        SSL_load_error_strings ();
        // Register the available ciphers and digests
        SSL_library_init ();

        // New context saying we are a server, and using SSL 2 or 3
        c->sslContext = SSL_CTX_new( TLSv1_2_server_method() );
        if (c->sslContext == NULL)
        {
            ERR_print_errors_fp (stderr);
            perror("Could not create context");
            return 0;
        }


        // Now I set a few options....
        /*SSL_CTX_set_verify(c->sslContext, NULL );*/
        // const long flags = 
        //   SSL_OP_NO_SSLv2 | SSL_OP_NO_SSLv3 | SSL_OP_NO_TLSv1 | SSL_OP_NO_SESSION_RESUMPTION_ON_RENEGOTIATION | SSL_OP_NO_COMPRESSION;
        const long flags = 
            SSL_OP_NO_SESSION_RESUMPTION_ON_RENEGOTIATION | SSL_OP_NO_COMPRESSION;
        SSL_CTX_set_options(c->sslContext, flags);
        setup_dh_parms( c->sslContext );
        SSL_CTX_set_ecdh_auto (c->sslContext, 1);

        // Give the impression that we are using SNI
        SSL_CTX_set_tlsext_servername_callback(c->sslContext, ssl_servername_cb);



        // The only cipher supported by HTTP/2 ... sort of.
        result = SSL_CTX_set_cipher_list(c->sslContext, "ECDHE-RSA-AES128-GCM-SHA256");
        /*result = SSL_CTX_set_cipher_list(c->sslContext, "DEFAULT");*/

        // Be sure we are able to do ALPN
        // void SSL_CTX_set_alpn_select_cb(SSL_CTX *ctx,
        //                           int (*cb) (SSL *ssl,
        //                                      const unsigned char **out,
        //                                      unsigned char *outlen,
        //                                      const unsigned char *in,
        //                                      unsigned int inlen,
        //                                      void *arg), void *arg);
        SSL_CTX_set_alpn_select_cb( c->sslContext, protocol_select, c);

        if ( result != 1 )
        {
            ERR_print_errors_fp (stderr);
            perror("Could not set cipher");
            return 0;
        }
        // Load a certificate in .pem format.
        SSL_CTX_use_certificate_chain_file(c->sslContext, certificate_filename);
        SSL_CTX_use_PrivateKey_file(c->sslContext,  privkey_filename, SSL_FILETYPE_PEM);
        // Check the private key 
        result = SSL_CTX_check_private_key(c->sslContext);
        if ( result != 1)
        {
            char msg[600];
            snprintf(msg, 600, "Check certificate file %s and privkey %s failed", certificate_filename,
                     privkey_filename);
            perror(msg);
            return 0;
        }
        // //////////////////////////////////////////

    }
    else
    {
        perror ("Connect failed");
    }

    return c;
}


connection_t* make_connection(char* certificate_filename, char* privkey_filename, char* hostname, int portno,
        char* my_protocol_list, int protocol_list_length
        )
{
    const int len = strlen(hostname);
    if ( len > 0 && hostname[len-1] == '\n' )
    { 
        printf("HELLO THERE. I found some weird characters\n");
        *( (int*) 0) = 42; // <-- This is the answer
    }


    return sslStart(
            certificate_filename, privkey_filename, hostname, 
            portno, my_protocol_list, protocol_list_length); 
}

int wait_for_connection(
        connection_t* c, 
        int microseconds,
        wired_session_t** wired_session)
{
    int clilen, newsockfd;
    // Don't return anything if there's a failure...
    *wired_session = 0;

    // Wait for a connection
    struct sockaddr_in cli_addr;
    clilen = sizeof(cli_addr);

    // Use select to get "interruptible" accepts 
    fd_set rfds;

    int can_go = 0;

    while( ! can_go )
    {
        FD_ZERO(&rfds);
        FD_SET(c->socket, &rfds);
        struct timeval tv;
        tv.tv_sec = microseconds / 1000000;
        tv.tv_usec = microseconds % 1000000;


        int retval = select(FD_SETSIZE, &rfds, NULL, NULL, &tv);


        if ( retval == -1 )
        {
            if ( errno == EINTR ) 
            {
                // printf(".");
                // I get a lot of these signals due to the fact of the 
                // Haskell runtime having its own use for signals.
                return TIMEOUT_REACHED;
            } else {
                perror("select()");
                return BAD_HAPPENED;
            }
        } else if (retval > 0)
        {
            // We got data, just let it go...
            // printf("letitgo\n");
            can_go = 1;
        } else 
        {
            // We didn't get data, finish and terminate
            // printf("timeo\n");
            return TIMEOUT_REACHED;
        }

    }

    wired_session_t* result = (wired_session_t*) malloc(sizeof(wired_session_t) );
    if ( result == 0 )
    {
        perror("Malloc failed");
        return BAD_HAPPENED;
    }

    /* Accept actual connection from the client */
    newsockfd = accept(c->socket, (struct sockaddr *)&cli_addr, &clilen);

    if (newsockfd < 0)
    {
        perror("ERROR on accept");
        return BAD_HAPPENED;
    }

    // Create an SSL struct for the connection

    result->socket = newsockfd;
    result->sslHandle = SSL_new (c->sslContext);
    if (result->sslHandle == NULL)
    {
        perror("No handle on SSL new");
        ERR_print_errors_fp (stderr);
        return BAD_HAPPENED;
    }

    // Connect the SSL struct to our connection
    if (!SSL_set_fd (result->sslHandle, result->socket))
    {
        perror("Could not associate");
        ERR_print_errors_fp (stderr);
        return BAD_HAPPENED;
    }

    // Initiate SSL handshake
    if (SSL_accept (result->sslHandle) != 1)
    {
        perror("Could not accept");
        ERR_print_errors_fp (stderr);
        return BAD_HAPPENED;
    }

    // After this one is okej, see which protocol was selected
    const unsigned char* out_protocol; unsigned pr_len;
    SSL_get0_alpn_selected(result->sslHandle, &out_protocol,
            &pr_len);
    // Wonder who is in charge of releasing that buffer...
    result -> protocol_index = lookup_protocol( 
            (char*)out_protocol, pr_len, 
            c->protocol_list, c->protocol_list_length);

    *wired_session = result;

    return ALL_OK;
}

// Disconnect & free connection struct
/*static void sslDisconnect (connection *c)*/
/*{*/
/*if (c->socket)*/
/*close (c->socket);*/
/*if (c->sslHandle)*/
/*{*/
/*SSL_shutdown (c->sslHandle);*/
/*SSL_free (c->sslHandle);*/
/*}*/
/*if (c->sslContext)*/
/*SSL_CTX_free (c->sslContext);*/

/*free (c);*/
/*}*/


int send_data(wired_session_t* ws, char* buffer, int buffer_size)
{
    if ( buffer_size == 0 )
    {
        return ALL_OK;
    }
    if (ws)
    {
        int result = SSL_write( ws->sslHandle, buffer, buffer_size);
        if ( result > 0 )
        {
            return ALL_OK;
        } else {
            return BAD_HAPPENED;
        }
    } else 
    {
        return BAD_HAPPENED;
    }
}

int recv_data(wired_session_t* ws, char* inbuffer, int buffer_size, int* data_recvd)
{
    int received=0, count = 0;
    char buffer[1024];

    // printf("Recvd entered\n");

    if (ws)
    {
        received = SSL_read (ws->sslHandle, inbuffer, buffer_size);
    }
    if ( received <= 0 )
    {
        ERR_print_errors_fp (stderr);
        return BAD_HAPPENED;
    }
    *data_recvd = received ;

    // printf("Recvd exited\n");

    return ALL_OK;
}

#define MUTEX_TYPE       pthread_mutex_t
#define MUTEX_SETUP(x)   pthread_mutex_init(&(x), NULL)
#define MUTEX_CLEANUP(x) pthread_mutex_destroy(&(x))
#define MUTEX_LOCK(x)    pthread_mutex_lock(&(x))
#define MUTEX_UNLOCK(x)  pthread_mutex_unlock(&(x))
#define THREAD_ID        pthread_self(  )


void handle_error(const char *file, int lineno, const char *msg){
    fprintf(stderr, "** %s:%d %s\n", file, lineno, msg);
    ERR_print_errors_fp(stderr);
    /* exit(-1); */ 
}

/* This array will store all of the mutexes available to OpenSSL. */ 
static MUTEX_TYPE *mutex_buf= NULL;


static void locking_function(int mode, int n, const char * file, int line)
{
    if (mode & CRYPTO_LOCK)
        MUTEX_LOCK(mutex_buf[n]);
    else
        MUTEX_UNLOCK(mutex_buf[n]);
}

static unsigned long id_function(void)
{
    return ((unsigned long)THREAD_ID);
}

void dispose_wired_session(wired_session_t* ws)
{
    if (ws == 0)
        return ;
    if ( ws-> sslHandle )
    {
        SSL_shutdown( ws->sslHandle );
        ws -> sslHandle = 0;
    }
    if (ws->socket)
    {
        close(ws->socket);
        ws -> socket = 0;
    }
    free(ws);
}


int thread_setup(void)
{
    int i;

    // printf("Threads setup\n");

    mutex_buf = malloc(CRYPTO_num_locks(  ) * sizeof(MUTEX_TYPE));
    if (!mutex_buf)
        return 0;
    for (i = 0;  i < CRYPTO_num_locks(  );  i++)
        MUTEX_SETUP(mutex_buf[i]);
    CRYPTO_set_id_callback(id_function);
    CRYPTO_set_locking_callback(locking_function);
    return 1;
}

int thread_cleanup(void)
{
    int i;

    if (!mutex_buf)
        return 0;
    CRYPTO_set_id_callback(NULL);
    CRYPTO_set_locking_callback(NULL);
    for (i = 0;  i < CRYPTO_num_locks(  );  i++)
        MUTEX_CLEANUP(mutex_buf[i]);
    free(mutex_buf);
    mutex_buf = NULL;
    return 1;
}

