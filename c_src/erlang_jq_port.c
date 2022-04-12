#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include "erlang_jq_port_process.h"

typedef unsigned char byte;

FILE *log_file;
int read_exact(byte *buf, int len)
{
  int i, got = 0;

  do {
      if ((i = read(0, buf+got, len-got)) <= 0) {
          fprintf(log_file, "HMM CONNECTION CLOSED? %d\n", i);
          return i;
      }
    got += i;
  } while (got<len);

  return len;
}

int write_exact(byte *buf, int len)
{
  int i, wrote = 0;

  do {
    if ((i = write(1, buf+wrote, len-wrote)) <= 0)
      return i;
    wrote += i;
  } while (wrote<len);

  return len;
}

typedef unsigned char byte;

byte* read_cmd()
{
#define SIZE_LEN 4
    // TODO: change len to fixed size type
    size_t len = 0;
    byte buf[SIZE_LEN]; 
    if ((len = read_exact(buf, SIZE_LEN)) != SIZE_LEN) {
        return NULL;
    }
    len = 0;
    for (int i = (SIZE_LEN-1); i >= 0; i--) {
        size_t addad_byte = ((size_t)buf[SIZE_LEN - (i+1)] << (i*sizeof(byte)));
        len = len | addad_byte;
    }
    byte* command_content = malloc(len);
    if (read_exact(command_content, len) != len) {
        free(command_content);
        return NULL;
    }
    return command_content;
}

int write_cmd(byte *buf, size_t len)
{
  byte li;

  for (int i = 0; i < SIZE_LEN; i++) {
      li = (len >> (8 * (SIZE_LEN - (i+1)))) & 0xff;
      write_exact(&li, 1);
  }

  return write_exact(buf, len);
}

static int handle_process_json() {
     byte * jq_program = read_cmd();
     if (jq_program == NULL) {
        return 0;
     }
     byte * json_data = read_cmd();
     if (json_data == NULL) {
        return 0;
     }
    fprintf(log_file, "%s\n", jq_program);
    fprintf(log_file, "%s\n", json_data);
    PortString_dynarr result_strings;
    PortString_dynarr_init(&result_strings);
    char* error_msg = NULL;
    int res = erlang_jq_port_process_json(
            (char*)jq_program,
            (char*)json_data,
            0,
            512,
            &result_strings,
            &error_msg);
    if (res == JQ_OK) {
        
        fprintf(log_file, "WRITTING OK!!\n");
        int bytes_written = write_cmd((byte*)err_tags[JQ_OK], strlen(err_tags[JQ_OK]));
        fprintf(log_file, "WROTE %d BYTES for the OK message\n", bytes_written);
        size_t nr_of_result_objects = PortString_dynarr_size(&result_strings);
        char buf[64];
        sprintf(buf, "%lu", nr_of_result_objects);
        write_cmd((byte*)buf, strlen(buf));

        for (int i = 0; i < nr_of_result_objects; i++) {
            PortString result = PortString_dynarr_item_at(&result_strings, i);
            write_cmd((byte*)result.string, result.size);
            free(result.string);
        }
    } else {
        // Error message
        fprintf(log_file, "WRITTING ERROR RESPONESE!!\n");
        const char* error_str = "error";
        write_cmd((byte*)error_str, strlen(error_str));
        write_cmd((byte*)err_tags[res], strlen(err_tags[res]));
        write_cmd((byte*)error_msg, strlen(error_msg));

    }
    fprintf(log_file, "RESULT: %s\n", PortString_dynarr_item_at(&result_strings, 0).string);
    return 1;
}

int main() {
erlang_jq_port_process_init();
  byte buf[100];
  log_file = fopen("./test.txt", "w+");
  while (1) {
      byte * command = read_cmd();
      if (command == NULL) {
        return 1;
      }
      if (strcmp((char*)command, "process_json") == 0) {
            free(command);
            if(!handle_process_json()) {
                return 1;
            }
      } else {
        // Unknown command
          return 1;
      }
      sprintf((char*)buf, "ok");
      write_exact(buf, strlen((char*)buf));
    /* fn = buf[0]; */
    /* arg = buf[1]; */
    
    /* if (fn == 1) { */
    /*   res = foo(arg); */
    /* } else if (fn == 2) { */
    /*   res = bar(arg); */
    /* } */

    /* buf[0] = res; */
    /* write_cmd(buf, 1); */
  }
}
