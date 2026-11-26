# Prelude Java

!!! Note

    This module builds on top of the shared [Programming](programming.md)
    module and the [LSP](lsp.md) module.

## Package Prerequisites

All you need on your system is a JDK. The language server
([Eclipse JDT LS](https://github.com/eclipse-jdtls/eclipse.jdt.ls)) is
downloaded for you the first time you open a Java file.

Keep in mind that JDT LS is a Java program itself, and recent releases
are picky about the JDK they run on (at the time of writing the latest
one needs Java 25). That's only about running the server - your
projects can still target older Java versions. If the server dies right
after it starts, an old `java` on your `PATH` is the most likely reason.

## Java Mode

The module uses the tree-sitter powered `java-ts-mode` that comes with
Emacs. Prelude offers to install the `java` grammar the first time you
open a Java file (see `prelude-treesit-auto-install`). Without the
grammar you get the classic `java-mode`.

`subword-mode` is enabled, so the editing commands understand
`CamelCase`.

## LSP support

Code completion, navigation, refactoring and diagnostics all come from
JDT LS. How it's wired up depends on `prelude-lsp-client`.

### Eglot

Eglot can start JDT LS by itself when there's a `jdtls` on your `PATH`,
but that's about it. With Eglot (the default) the module uses
[eglot-java](https://github.com/yveszoundi/eglot-java) instead, which
takes care of a few things Eglot doesn't do on its own:

- it installs JDT LS (in `eglot-java-server-install-dir`) and starts it
- <kbd>M-.</kbd> works for the classes of the JDK and your
  dependencies, not only for your own code
- it runs JUnit tests and main classes
- it creates new projects (Maven, Gradle, Spring, Quarkus, Micronaut
  and Vert.x)

Some commands worth knowing:

- `eglot-java-project-new` - create a new project
- `eglot-java-file-new` - create a new class, interface, enum or record
- `eglot-java-project-build-task` - run a Maven or Gradle task
- `eglot-java-project-build-refresh` - tell the server about changes to
  your build file
- `eglot-java-upgrade-lsp-server` - upgrade JDT LS

The sources of library classes you jump to are saved in an
`.eglot-java` folder in your project, so you'll probably want to add
that to your global gitignore.

If the JDK you'd like the server to run on isn't the first `java` on
your `PATH`, point eglot-java at it:

```emacs-lisp
(setq eglot-java-java-program "/path/to/jdk/bin/java")
```

### lsp-mode

With lsp-mode the module uses
[lsp-java](https://emacs-lsp.github.io/lsp-java/), which installs JDT
LS as well. It comes with `dap-java` for running tests and debugging,
and a lot of extras (a test browser, a dependency browser, Spring Boot
support, etc).

## Key Bindings

<!-- markdownlint-disable MD013 -->

| Key | Eglot | lsp-mode | Description |
| --- | ----- | -------- | ----------- |
| <kbd>C-c C-t</kbd> | `eglot-java-run-test` | `dap-java-run-test-method` | Run the test method at point, or the whole test class |
| <kbd>C-c C-r</kbd> | `eglot-java-run-main` | `dap-java-debug` | Run the main class |

<!-- markdownlint-enable MD013 -->

With Eglot a prefix argument (<kbd>C-u</kbd>) starts the JVM in debug
mode, waiting for a debugger to attach on port 8000. With lsp-mode
<kbd>C-c C-r</kbd> runs the program in the debugger.

See the [LSP](lsp.md) module for the rest of the LSP key bindings
(rename, code actions, organize imports, etc).

## Building projects

[Projectile](https://github.com/bbatsov/projectile) knows about Maven
and Gradle projects, so the usual commands work out of the box:

- <kbd>C-c p c c</kbd> - compile the project
- <kbd>C-c p P</kbd> - run the tests
- <kbd>C-c p u</kbd> - run the project
- <kbd>C-c p t</kbd> - switch between a class and its test

Prelude installs `groovy-mode` and `kotlin-mode` the first time you
open a Gradle build file.

## Formatting

<kbd>C-c L f</kbd> formats the buffer with JDT LS. If you'd rather
have format-on-save, the [Apheleia](apheleia.md) module supports
[google-java-format](https://github.com/google/google-java-format) out
of the box.
