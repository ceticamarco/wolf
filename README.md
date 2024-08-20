# Wolf 🐺

**Wolf** is a configurable file watchdog for Linux platform written in C. **Wolf** monitors 
a set of files or directories and prints out a log event each time the watched resources changes. The watchdog
can be configured to monitor any kind of event, that includes file creation and deletion, file moving, I/O and
permission changes. Additionally, **Wolf** can execute an user-defined command every time a watchdog detects a
change, thus allowing you to easily build complex pipelines without the need to employ any additional tool.

**Wolf** relies on the `inotify(7)` system call, therefore it is only compatible with Linux-based systems.

## Building
The single source file(`wolf.c`) of the watchdog can be compiled using any C99 compiler. To build it, issue the following command:
```sh
$> make clean all
```

This command will produce a statically compiled binary called *wolf*.

## Usage
```
Wolf - Configurable file watchdog for Linux platform.

Syntax: './wolf [-c|-d|-m|-r|-w|-p|-f|-e] <PATH ...>'
options:
-c, --create              | Add a watchdog for file creation
-d, --delete              | Add a watchdog for file deletion
-m, --move                | Add a watchdog for file movements or file renaming
-r, --read                | Add a watchdog for reading events
-w, --write               | Add a watchdog for writing events
-p, --permission          | Add a watchdog for permissions changes
-f, --full                | Enable all the previous options
-e, --exec                | Execute a command when a watchdog detects a change
--no-timestamp            | Disable timestamp from watchdog output
-v, --version             | Show program version
-h, --help                | Show this helper

General help with the software: https://git.marcocetica.com/marco/wolf
Report bugs to: Marco Cetica(<email@marcocetica.com>)
```

**Wolf** is pretty straightforward to use. It requires at least one watchdog option and at least
one file/directory to watch as command line arguments. For example, to watch the local files `foo`, `bar` and 
the directory `src/` for _reading_, _writing_ and _deletion_ events, issue the following command:

```sh
$> ./wolf -rwd foo bar src
```

The watchdog will start polling the resources for the specified events and will log on the standard output using
the following pattern:

```
[<timestamp>] <event> '<path>' (<filetype>)
```

Where `<timestamp>` is the _timestamp_ of the event, `<event>` is the _event type_, `<path>` is the _filename_ of
the watched resource and `<filetype>` is the _type_ of the resource in the watchlist.

The `<event>` field is any of the following token: **C**, **D**, **M**, **R**, **W**, **P**.

For instance, if you try to read one of the files of the previous example(`cat foo`), **wolf** would produce the following
output:

```
[2024-07-29 20:24:52] R 'foo' (file)
```

A write syscall to the `src/test` file would instead produce the following log:

```
[2024-07-29 20:26:20] W 'src/test' (file)
```

You can also choose to watch a directory by specifying its path:

```sh
$> ./wolf -rwd $PWD
```

This command will add a watchdog to the current directory for events of the type _"read"_, _"write"_ and _"delete"_
generated for any file or directory on the current path. Do note that this command is **NOT** recursive(see the caveats section for more information).

Additionally, you can also tell **wolf** to add a watchdog to _any_ kind of event by using the `-f, --full` option:

```sh
%> ./wolf --full $PWD
```

Which is equivalent to `./wolf -cdmrwp $PWD`. Finally, you can also force **wolf** to disable the timestamp output by using the `--no-timestamp` option:

```sh
%> ./wolf -f --no-timestamp $PWD
```

This would produce the following output:

```
R '/home/marco/wolf' (dir)
R '/home/marco/wolf/foo' (file)
D '/home/marco/wolf/src' (dir)
R '/home/marco/wolf' (dir)
P '/home/marco/wolf/a.out' (file)
W '/home/marco/wolf/a.out' (file)
```
Additionally, if you want to execute a custom command every time a watchdog detects a change, you can do so by
using the `-e,--exec` option. For instance, suppose that you have a Python file(`foo.py`) with the following content:

```py
def square(x):
    return x ** 2

print(f"10^2 = {square(10)}")
```

and you want to continously evaluate it as soon as you save it to the disk. To do this, you can use **Wolf** as described
below:

```sh
$> ./wolf -w --exec 'python foo.py'
```

Each time a write event is detected by the watchdog, the supplied command will be issued, causing the program 
to be automatically evaluated, that is:

```sh
$> ./wolf -w --exec 'python foo.py'
[2024-08-20 16:24:43] W 'foo.py' (file)
10^2 = 100
[2024-08-20 16:24:55] W 'foo.py' (file)
10^2 = 100
5^2 = 25
[2024-08-20 16:25:10] W 'foo.py' (file)
10^2 = 100
5^2 = 25
4^2 = 16
```

Be sure to read the _"technical details"_ section to learn more about the concurrent aspects of this feature and how **Wolf**
spawns a new process.

## Technical details
Below there is a brief list of the things you should be aware of when using **Wolf**.

- The `-e,--exec` option works by spawning a child process using the `fork(2)` system call; thus, the command is being executed
in a new process;  
- The `-e,--exec` option is a **NON-BLOCKING** feature, meaning that the parent process will continue to log new changes while the
child process execute the supplied command; therefore the parent process will **NOT** wait for the child(s) process to terminate;  
- Since the parent process does not await for the child process to complete it will also not handle its return code, thus the exit
status of any supplied command is ignored.
- Any `SIGCHLD` signal generated by a child process is ignored, therefore the reaping of any child process is delegated to the kernel;  
- `inotify` is **NOT** recursive. Meaning that you cannot monitor subdirectories of a watched directory;  
- `inotify` can only work within files for which you already have reading and writing permissions;  
- `inotify` removes deleted files from the `inotify_add_watch(2)`, meaning that, after a file is being deleted, the watchdog associated with it 
is automatically removed as well. To add it again, the program has to be restarted;  
- `inotify` is quite verbose by design. For instance if you try to write to a **non-empty** watched file
using the `echo(1)` command along with a _redirection_(i.e., `echo 'hello world' > foo`), the watchdog will
log two events:

```
W '/home/marco/wolf/foo' (file)
W '/home/marco/wolf/foo' (file)
```
This is because the previous command makes two system calls: one to `truncate(1)` the file at zero length and
the other to `write(1)` the content into the file. The former is issued by the shell itself while the latter is
performed by the `echo(1)` command. You can detect these syscalls by using the `strace(1)` command:

```sh
$> strace sh -c 'echo "hello world" > foo'
execve("/usr/bin/sh", ["sh", "-c", "echo \"hello world\" > foo"], 0xffffd10381d0 /* 23 vars */) = 0
# Suppressed output
openat(AT_FDCWD, "foo", O_WRONLY|O_CREAT|O_TRUNC, 0666) = 3 # <-- Truncate system call(first event)
fcntl(1, F_DUPFD, 10)                   = 10
close(1)                                = 0
fcntl(10, F_SETFD, FD_CLOEXEC)          = 0
dup3(3, 1, 0)                           = 1
close(3)                                = 0
write(1, "hello world\n", 12)           = 12 # <-- Write system call(second event)
dup3(10, 1, 0)                          = 1
close(10)                               = 0
exit_group(0)                           = ?
+++ exited with 0 +++
```

Since `inotify(1)` intercepts both, **wolf** will also log the same operation twice.

## License
[GPLv3](https://choosealicense.com/licenses/gpl-3.0/)
