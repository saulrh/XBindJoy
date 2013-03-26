#include <config.h>
#include <stdio.h>

#include <libguile.h>

/* structure: */
/*   xbindjoy contains the event loop and as little else as possible */
/*   joystick contains functions for handling joystick data */
/*   sender contains functions for sending x events */
/*   guile contains functions to handle the config script */


void inner_main(void* data, int argc, char** argv) {
    printf("foo!");
    scm_shell(argc, argv);
}
          
int main(int argc, char** argv) {
    scm_boot_guile(argc, argv, (inner_main), NULL);
}
