About Colleen
-------------
Colleen is a relatively large IRC bot framework with support for event handling, encapsulation through modules, command parsing, threading and more. However, it is also usable as a direct IRC bot straight away thanks to a plethora of modules that are included by default and should provide most of the functionality you might want out of a bot.

How to Use Colleen as a Bot
---------------------------
Load Colleen through Quicklisp or ASDF:

```
(ql:quickload :colleen)
(in-package :colleen)
```

And start it up!

```
(startup)
```

Colleen is managed through a bunch of configuration files. By default it has servers configured for TymoonNET and Freenode. First though, let's change the default nick to something that is less likely to be taken already. With that done we'll save our configuration to a file.

```
(setf (bot-config :servers :default :nick) "Colleek")
(save-config)
```

This should save a file called `colleen.uc.lisp` to a `config` folder underneath the Colleen root folder. When you open the file you'll see it is organised using a lisp-like syntax, with extension for hash-tables. Another thing you'll notice is that all strings are preceded with a `-`. This is a necessary evil in order to serialise certain data types to string and be able to serialise them back. For more information on how the configuration storage works, see [Universal-Config](http://shinmera.github.io/Universal-Config/). Using the configuration file you can set the servers to connect to by default, which channels to join and so on. For now we'll do it manually with some code.

```
(connect :freenode)
```

If all works nicely, you'll see some log messages informing you of the connection. You should now be able to fire up an IRC client to Freenode and start conversing with your bot. Since the `essentials` module is loaded by default, you can try commands like `!time`, or `!echo hi`. If you want to see what's going on behind the scene, you can change the logging level from `:info` to `:trace`:

```
(setf (v:repl-level) :trace)
```

This'll show you quite a bit of output, including pings and all forms of event handling. It'll likely be much too noisy, but it can be very useful for debugging purposes.

Since Colleen is not just a bot, but also a framework, we can use its functions to communicate ourselves.

```
(irc:privmsg "this-is-my-nick-here" "Hello REPL!" :server (server :freenode))
```

Obviously change the nick to your own first.

Colleen's functionality is managed through modules. By default the `auth` and `essentials` modules are loaded. You can load up more modules by putting them into the `:MODULES` list in the configuration, using the `!module load` command or directly using `load-module`.

```
(load-module :silly)
```

Loading a module won't make it active yet though. For that you need to `start` it first. You can use either the command `!module start` or the `start-module` function. If you put it into the configuration instead, it'll also automatically start it for you when you `startup` Colleen.

```
(start-module :silly)
```

Some modules like `silly` perform actions not on commands, but when certain things are said. In order to avoid causing unnecessary noise, you need to activate for a specific channel. You can usually do that with an `activate` sub command. In this case it would be `!silly activate`. When you try that now you will most likely be denied access. Colleen includes a simple permissions restriction in order to separate "dangerous administrative commands" from publicly available ones. The `auth` module that is present by default will authenticate users by either a simple password or through NickServ. First you need to put your nick on the list of recognised names though. The `auth` module has its separate configuration file that currently does not exist yet. Before setting that up, we'll simply bypass it by smuggling our nick directly onto the authenticated list:

```
(push "this-is-my-nick-here" (auth-users (get-server :freenode)))
```

You should now be able to invoke `!silly activate` and try it with saying something like `how the hell`. You can stop it any time by invoking `!silly deactivate` or stopping the module entirely `!module stop silly`.

Now in order to get the authentication set up properly, we first need the configuration file to appear. In order to manage configuration files for modules you can use `!module storage save auth` or `save-storage`.

```
(save-storage (module :auth))
```

The file will most likely just contain a table stub. Change it to something like this to make yourself authenticate-able:

```
{EQL
:LOGINS {EQL
         :this-is-my-nick-here "-myPasswordGOESheeere!"}}
```

Don't forget the necessary "-" before your password. Next you can load the configuration back in using `!module storage load auth` or `load-storage`.

```
(load-storage (module :auth))
```

Using `!logout` you can remove yourself from the authenticated list. If your nick is registered with NickServ you should now be able to use just `!login` to log back in, or alternatively pass your password along.

Since everything is managed through configuration files you can create a running standalone bot by simply starting your lisp implementation with a file containing something akin to the following in it

```
(ql:quickload :colleen)
(funcall (find-symbol "STARTUP" "COLLEEN"))
```

Colleen has a lot more to offer of course, but I think this should suffice as a setup guide. For all the different modules, have a look at the `modules/` directory in Colleen's root. The commands each of them offer should be clear enough in the source, but if you prefer a more straight forward explanation of all the public functionality, there is a guide [here](http://plaster.tymoon.eu/view/5A). You can of course also experiment with the functionality without setting up your own and just using the live instance as mentioned in the guide.

To shut Colleen down again nicely, simply invoke `shutdown`.

Using Colleen as a Framework
----------------------------
Functions are encapsulated in modules. A module requires a package and a `define-module` form within that package. A simple module stub would look something like this:

```
(in-package #:colleen)
(defpackage #:my-module
  (:use #:cl #:colleen #:events))
(in-package #:my-module)

(define-module my-module () ())
```

The `define-module` form is the same as `defclass`, with some additional side effects to register it with the system. The extra package is necessary in order to provide an implicit environment to carry the current module and make commands like `define-command` and `define-handler` shorter. It is not strictly necessary to use a separate package per module, but it certainly is a good idea.

Generally you'll want to use `define-command`, `define-group` and `define-handler`, although there exist more low-level constructs that allow you to do the same with more control over the output.

```
(define-group my-commands :documentation "A collection of commands for my test module!")

(define-command (my-commands test) () (:documentation "Simply performs a test.")
  (respond event "Testing, testing, 1, 2, 3.")

(define-handler (privmsg-event event) ()
  (v:info :my-module "Got a privmsg by ~a: ~a" (nick event) (message event)))
```

By default within a `define-command` or `define-handler` body, the symbols `event` and `module` are bound to the current event instance and the current module instance. Furthermore, the module's storage is bound to `uc:*config*`. Especially the configuration is useful, as you don't need to handle that manually yourself. Colleen automatically provides storage for you and will save and load it accordingly when your module is loaded or started. Accessing the configuration happens through Universal-Config's `config-tree`, which is `setf`-able. For more information on how serialising and accessing and all that works, see the [Universal-Config documentation](http://shinmera.github.io/Universal-Config/). For now we'll use this to implement a simple `last-message` function:

```
(define-handler (privmsg-event event) ()
  (unless (command-p (message event))
    (setf (uc:config-tree :last-message) (message event))))

(define-command last-message () ()
  (respond event (uc:config-tree :last-message)))
```

Aside from commands and handlers, Colleen also includes a timer functionality in order to schedule tasks. See `define-timer` and `schedule-timer`. Generally having a look at all the already existing modules should give a good idea on how to make your own, they usually aren't big and shouldn't be hard to understand in what they do.
