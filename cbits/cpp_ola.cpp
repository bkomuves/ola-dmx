

// g++ -c cpp_ola.cpp $(pkg-config --cflags --libs libola)
//
// > echo $(pkg-config --cflags --libs libola)
// -D_THREAD_SAFE -I/usr/local/Cellar/ola/0.10.1/include -I/usr/local/Cellar/protobuf/2.6.1/include -L/usr/local/Cellar/ola/0.10.1/lib -L/usr/local/Cellar/protobuf/2.6.1/lib -lola -lolacommon -lprotobuf -D_THREAD_SAFE
//

#include <stdlib.h>
#include <unistd.h>
#include <ola/DmxBuffer.h>
#include <ola/Logging.h>
#include <ola/client/StreamingClient.h>
#include <iostream>

// -----------------------------------------------------------------------------

extern "C" {
  
void ola_init_logging();

struct DMX *ola_new_buffer(int universe);
void ola_free_buffer( DMX *p );
int ola_buffer_size( DMX *p );

void ola_set_channel( DMX *p , int channel , int value );
void ola_set_buffer_at( DMX *p, int start , int len , int *values);
void ola_set_buffer   ( DMX *p ,            int len , int *values);
int ola_send_buffer( DMX *p );
int ola_send_values( DMX *p , int len , int *values);  
  
}

// -----------------------------------------------------------------------------

struct DMX
{ 
  int                           dmx_universe;
  ola::DmxBuffer               *dmx_buffer; 
  ola::client::StreamingClient *dmx_client;
};

// -----------------------------------------------------------------------------

// turn on OLA logging
void ola_init_logging()
{
  ola::InitLogging(ola::OLA_LOG_WARN, ola::OLA_LOG_STDERR);
}

// note: universe numbering starts from 1
struct DMX *ola_new_buffer(int universe)
{
  DMX *p = (DMX*) malloc( sizeof(DMX) );
    
  p -> dmx_universe = universe;

  // create a client
  p -> dmx_client   = new ola::client::StreamingClient( ola::client::StreamingClient::Options() );
  int status = p -> dmx_client -> Setup();
  if (!status)
  { 
    // error
    return 0;
  }

  // create a buffer
  p -> dmx_buffer   = new (ola::DmxBuffer);
  p -> dmx_buffer -> Blackout(); // Set all channels to 0
  
  return p;
}

void ola_free_buffer( DMX *p )
{
  if (p != 0)
  {
    delete (p -> dmx_buffer);
    free(p);
  }  
}

int ola_buffer_size( DMX *p )
{
  return (p -> dmx_buffer -> Size());
}

void ola_set_channel( DMX *p , int channel , int value )
{
  p -> dmx_buffer -> SetChannel( channel , value & 0xff );
}

void ola_set_buffer_at( DMX *p, int start , int len , int *values)
{
  ola::DmxBuffer *buf = p -> dmx_buffer;
  for (int i=0;i<len;i++)
  {
    buf -> SetChannel( start + i , values[i] & 0xff );
  }
}

void ola_set_buffer( DMX *p , int len , int *values)
{
  ola_set_buffer_at( p , 0 , len , values );
}

int ola_send_buffer( DMX *p )
{
  int status = p -> dmx_client -> SendDmx( p -> dmx_universe , *(p -> dmx_buffer) );
  return status;
}

int ola_send_values( DMX *p , int len , int *values)
{
  ola_set_buffer ( p , len , values );
  return ola_send_buffer( p );   
}

// -----------------------------------------------------------------------------

