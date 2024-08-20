#define _GNU_SOURCE

#ifndef __linux__
    #error "Unsupported platform"
#endif

#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <stdlib.h>
#include <getopt.h>
#include <poll.h>
#include <time.h>
#include <sys/inotify.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <stdint.h>
#include <signal.h>

#define INOTIFY_EVENT_INC sizeof(struct inotify_event) + event->len
#define WATCHDOG_FORMAT "[%s] %c '%s' (%s)\n"
#define WATCHDOG_FORMAT_NOTS "%c '%s' (%s)\n"
#define FILE_FMT "%s/%s"
#define DIR_FMT "%s"

typedef enum {
    E_CREATE = 'C',
    E_DELETE = 'D',
    E_MOVE = 'M',
    E_READ = 'R',
    E_WRITE = 'W',
    E_PERM = 'P',
    E_UNDEF = 0xB00B5
} watchdog_event;

typedef enum { false, true } bool;

static void handle_inotify_events(int fd, const int *wd, int wd_len, char **watched_files, const bool is_timestamp_enabled, const char *watchdog_cmd);
static void get_timestamp(uint8_t *timestamp, const ssize_t timestamp_len);
static void exec_command(const char *cmd);
static uint8_t **tokenize_command(const char *cmd);

volatile sig_atomic_t stop_signal = 0;
void sigint_handler() {
    stop_signal = ~stop_signal;
}

void helper(const char *name) {
    printf("Wolf - Configurable file watchdog for Linux platform.\n\n"
           "Syntax: '%s [-c|-d|-m|-r|-w|-p|-f] <PATH ...>'\n"
           "options:\n"
           "-c, --create              | Add a watchdog for file creation\n"
           "-d, --delete              | Add a watchdog for file deletion\n"
           "-m, --move                | Add a watchdog for file movements or file renaming\n"
           "-r, --read                | Add a watchdog for reading events\n"
           "-w, --write               | Add a watchdog for writing events\n"
           "-p, --permission          | Add a watchdog for permissions changes\n"
           "-f, --full                | Enable all the previous options\n"
           "-e, --exec                | Execute a command when a watchdog detects a change\n"
           "--no-timestamp            | Disable timestamp from watchdog output\n"
           "-v, --version             | Show program version\n"
           "-h, --help                | Show this helper\n\n"
           "General help with the software: https://git.marcocetica.com/marco/wolf\n"
           "Report bugs to: Marco Cetica(<email@marcocetica.com>)\n", name);
}

void version() {
    printf("Wolf (v%s) - Configurable file watchdog for Linux platform.\n"
           "Copyright (c) 2024 Marco Cetica\n"
           "License GPLv3+: GNU GPL version 3 or later\n\n"
           "Project homepage: <https://git.marcocetica.com/marco/wolf>.\n"
           "Email bug reports to: <email@marcocetica.com>.\n", VERSION);
}

int main(int argc, char **argv) {
    int opt, opt_idx = 0;
    const char *short_opts = "cdmrwpfvhe:";
    char *watchdog_cmd = NULL;
    uint32_t mask = 0;
    bool is_timestamp_enabled = true;
    struct option long_opts[] = {
        {"create",         no_argument, NULL, 'c'},
        {"delete",         no_argument, NULL, 'd'},
        {"move",           no_argument, NULL, 'm'},
        {"read",           no_argument, NULL, 'r'},
        {"write",          no_argument, NULL, 'w'},
        {"permission",     no_argument, NULL, 'p'},
        {"full",           no_argument, NULL, 'f'},
        {"exec",           required_argument, NULL, 'e'},
        {"no-timestamp",   no_argument, NULL,  0 },
        {"version",        no_argument, NULL, 'v'},
        {"help",           no_argument, NULL, 'h'},
        {NULL, 0, NULL, 0}
    };

    // Parse inotify options from command line
    while((opt = getopt_long(argc, argv, short_opts, long_opts, &opt_idx)) != -1) {
        switch(opt) {
            case 'c': mask |= IN_CREATE; break;
            case 'd': mask |= (IN_DELETE | IN_DELETE_SELF); break;
            case 'm': mask |= (IN_MOVE_SELF | IN_MOVED_FROM); break;
            case 'r': mask |= IN_ACCESS; break;
            case 'w': mask |= IN_MODIFY; break;
            case 'p': mask |= IN_ATTRIB; break;
            case 'f': mask = IN_CREATE | IN_DELETE | IN_DELETE_SELF | IN_MOVED_FROM |
                             IN_ACCESS | IN_MODIFY | IN_ATTRIB; break;
            case 'e': watchdog_cmd = optarg; break;
            case 0:
                if(!strcmp(long_opts[opt_idx].name, "no-timestamp")) {
                    is_timestamp_enabled = false;
                }
                break;
            case 'v': version(); return 0;
            case 'h': helper(argv[0]); return 0;
            default: helper(argv[0]); return 1;
        }
    }

    // Retrieve number of non-option arguments
    const int number_of_files = (argc - optind);

    // Check whether user has provided enough 
    // non-option arguments(i.e., files to watch)
    if(number_of_files == 0) {
        puts("Error: provide at least one file/directory to watch");
        helper(argv[0]);
        return 1;
    }

    // Check whether user has provided enough watchdog options
    if(mask == 0) {
        puts("Error: provide at least one watchdog option");
        helper(argv[0]);
        return 1;
    }

    // Register SIGINT handler
    signal(SIGINT, sigint_handler);

    // Initialize inotify API
    int fd = inotify_init1(IN_NONBLOCK);
    if(fd == -1) {
        perror("inotify_init1");
        exit(EXIT_FAILURE);
    }

    // Allocate enough memory for each watch descriptor
    int *wd = calloc(number_of_files, sizeof(int));
    if(wd == NULL) {
        perror("calloc");
        exit(EXIT_FAILURE);
    }

    // Register each file into the watchlist
    int file_idx = optind;
    for(size_t idx = 0; file_idx < argc; file_idx++, idx++) {
        wd[idx] = inotify_add_watch(fd, argv[file_idx], mask);
        if(wd[idx] == -1) {
            fprintf(stderr, "Cannot watch '%s': %s\n", argv[file_idx], strerror(errno));
            exit(EXIT_FAILURE);
        }
    }

    // Prepare polling
    nfds_t nfds = 1;
    struct pollfd fds[] = {
        { .fd = fd, .events = POLLIN }
    };

    while(!stop_signal) {
        int poll_num = poll(fds, nfds, -1);
        if(poll_num == -1) {
            if(errno == EINTR) {
                continue;
            }
            perror("poll");
            exit(EXIT_FAILURE);
        }

        if(poll_num > 0) {
            // Inotify events are available
            if(fds->revents & POLLIN) {
                handle_inotify_events(fd, wd, number_of_files, (argv + optind), is_timestamp_enabled, watchdog_cmd);


            }
        }
    }
    
    // Free allocated resources
    close(fd);
    free(wd);

    return 0;
}

static void handle_inotify_events(int fd, const int *wd, int wd_len, char **watched_files, const bool is_timestamp_enabled, const char *watchdog_cmd) {
    // Align inotify reading buffer to inotify_event struct
    char inotify_read_buf[4096]
        __attribute__((aligned((__alignof__(struct inotify_event)))));
    const struct inotify_event *event;

    while(1) {
        // Read events from inotify file descriptor
        ssize_t len = read(fd, inotify_read_buf, sizeof(inotify_read_buf));
        if(len == -1 && errno != EAGAIN) {
            perror("read");
            exit(EXIT_FAILURE);
        }

        // Exit whether read returns nothing
        if(len <= 0) {
            break;
        }

        // Handle each event of the buffer
        for(char *ptr = inotify_read_buf; ptr < (inotify_read_buf + len); ptr += INOTIFY_EVENT_INC) {
            // Retrieve single event
            event = (const struct inotify_event*)ptr;

            // Set the event type
            watchdog_event we = E_UNDEF;
            if(event->mask & IN_CREATE) {
                we = E_CREATE;
            } else if(event->mask & (IN_DELETE | IN_DELETE_SELF)) {
                we = E_DELETE;
            } else if(event->mask & IN_MOVED_FROM) {
                we = E_MOVE;
            } else if(event->mask & IN_ACCESS) {
                we = E_READ;
            } else if(event->mask & IN_MODIFY) {
                we = E_WRITE;
            } else if(event->mask & IN_ATTRIB) {
                we = E_PERM;
            } else if(event->mask & IN_IGNORED) {
                // This event is generated each time a watched file/directory is
                // deleted. The event is ignored since it adds no further information
                // about the watched file.
                continue;
            }

            // Print the watchdog event to the standard output
            uint8_t *file_name = NULL;
            size_t file_name_len;
            for(int i = 0; i < wd_len; i++) {
                if(wd[i] == event->wd) {
                    // Build filename
                    if(event->len) {
                        file_name_len = snprintf(NULL, 0, FILE_FMT, watched_files[i], event->name);
                        file_name = malloc((file_name_len+1) * sizeof(char));
                        if(file_name == NULL) {
                            perror("malloc");
                            exit(EXIT_FAILURE);
                        }

                        snprintf((char*)file_name, file_name_len+1, FILE_FMT, watched_files[i], event->name);
                    } else {
                        file_name_len = snprintf(NULL, 0, DIR_FMT, watched_files[i]);
                        file_name = malloc((file_name_len+1) * sizeof(char));
                        if(file_name == NULL) {
                            perror("malloc");
                            exit(EXIT_FAILURE);
                        }

                        snprintf((char*)file_name, file_name_len+1, DIR_FMT, watched_files[i]);
                    }

                    // Get file type
                    const char *file_type = (event->mask & IN_ISDIR ? "dir" : "file");

                    if(is_timestamp_enabled) {
                        // Get timestamp
                        uint8_t timestamp[20];
                        get_timestamp(timestamp, sizeof(timestamp));
                        // Print the result
                        printf(WATCHDOG_FORMAT, (char*)timestamp, we, file_name, file_type);
                    } else {
                        // Print the result without timestamp
                        printf(WATCHDOG_FORMAT_NOTS, we, file_name, file_type);
                    }
                    break;
                }
            }
            // If user supplied a command, execute it
            if(watchdog_cmd != NULL) {
                exec_command(watchdog_cmd);
            }

            free(file_name);
        }
        memset(inotify_read_buf, 0, sizeof(inotify_read_buf));
    }
}

static void get_timestamp(uint8_t *timestamp, const ssize_t timestamp_len) {
    time_t now = time(NULL);
    struct tm *timeinfo = localtime(&now);
    strftime((char*)timestamp, timestamp_len, "%Y-%m-%d %H:%M:%S", timeinfo);
}

static void exec_command(const char *cmd) {
    // Ignore SIGCHLD signals.
    // This allows us to avoid blocking the parent process until the child completes
    // its execution; furthermore, it allows us to prevent the creation of zombie processes
    // by delegating the cleanup process to the kernel. By doing so, we lose the ability 
    // to check the return status of the child process.
    signal(SIGCHLD, SIG_IGN);

    // Execute command in a new process
    pid_t pid = fork();
    
    if(pid == -1) {
        perror("fork");
        exit(EXIT_FAILURE);
    } else if(pid == 0) { // Child process
        // Tokenize command
        uint8_t **argv = tokenize_command(cmd);

        // Replace memory of child process with new program
        execvp((char*)argv[0], (char**)argv);

        // If execvp returns, it means it has failed
        switch(errno) {
            case ENOENT: puts("Cannot execute command: no such file or directory"); break;
            case EACCES: puts("Cannot execute command: permission denied"); break;
            default: puts("Cannot execute command"); break;
        }

        // Free allocated resources
        for (int i = 0; argv[i] != NULL; i++) {
            free(argv[i]);
        }
        free(argv);
        exit(EXIT_FAILURE);
    }
}

static uint8_t **tokenize_command(const char *cmd) {
    // Duplicate command
    char *cmd_dup = strdup(cmd);
    if(cmd_dup == NULL) {
        perror("strdup");
        exit(EXIT_FAILURE);
    }

    // Count number of arguments
    size_t argc = 0;
    char *token = strtok(cmd_dup, " ");
    while(token != NULL) {
        argc++;
        token = strtok(NULL, " ");
    }

    // Allocate enough memory for arguments(and null terminator)
    uint8_t **argv = malloc((argc + 1) * sizeof(uint8_t*));
    if(argv == NULL) {
        perror("malloc");
        exit(EXIT_FAILURE);
    }

    // Reset command string and tokenize again
    strcpy(cmd_dup, cmd);
    size_t idx = 0;
    token = strtok(cmd_dup, " ");
    while(token != NULL) {
        argv[idx] = (uint8_t*)strdup(token);
        if(argv[idx] == NULL) {
            perror("strdup");
            while(idx > 0) { free(argv[--idx]); }
            free(argv);
            free(cmd_dup);
            exit(EXIT_FAILURE);
        }
        idx++;
        token = strtok(NULL, " ");
    }

    // Null-terminate the string
    argv[idx] = NULL;
    // Clear temporary resources
    free(cmd_dup);

    return argv;
}
