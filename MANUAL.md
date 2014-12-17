Colleen User Manual
===================

Introduction
------------
Colleen is a bot that provides both useful and comedic functionality
for IRC channels. The software is written in Common Lisp and available
publicly in [open source format](https://github.com/Shinmera/colleen).
Help on developing the software would be greatly appreciated.

The live version this manual is about runs on a certain configuration
and as such some explanations would have to be adapted for different
configurations of the bot software. However, the general user does not
need to concern themself with this.

Where Can I Find Colleen
------------------------
The public instance running off the TyNET servers should be available
in the following Servers/Channels:

* TyNET/#Stevenchan
* TyNET/#bots
* Freenode/#lisp
* Freenode/#lispcafe
* Freenode/#lispweb
* Freenode/#lispgames
* Freenode/#thcrap
* Freenode/#touhou

About This Guide
----------------
This guide is written in [Markdown](http://daringfireball.net/projects/markdown/)
format. Additionally, all messages to the bot that are written as
examples will be surrounded by backquotes. The command names or
signatures (more on that later) will be written in UPPERCASE.

Some of the functionality described in here may be disabled or not
available on some of the channels listed above. In general it is assumed
that the functionality is tested in TyNET/#Stevenchan.

This guide only illustrates some public commands. There are a plethora of
commands that are only available to bot administrators to help manage the
bot's functions on the fly.

Commands
--------
The public instance is configured to listen for messages that are
prefixed with either ./ or ! . If such a message is encountered, it is
interpreted as a command to the bot. A full list of commands can only be
obtained by scanning through Colleen's source files, as there are too many
to list them all here.

Each command is defined by a pattern and a number of arguments specified
by a lambda-list. The pattern is usually merely the name of the command.
An example for this simplest form is TIME. To invoke the TIME command
Simply type `!time`. More complicated commands like the NOTIFY require
arguments to be supplied along with the name. The way these arguments are
described in the bot is in the form of a lambda-list (this list will be
shown to you if you inquire Colleen about a command).

I will illustrate the way lambda-lists are to be understood by example.
The following is an empty list and means no arguments: () . If the command
expects required arguments, they are listed one by one in the order they
need to be supplied in the list: (required also-required) You supply each
just in the way they are written in the lambda-list, space separated.
Furthermore, a lambda-list can contain optional arguments. These do not
need to be supplied, but if then still in order. In order to signify that
the remaining of the arguments are optional, the &optional keyword is shown:
(required-arg &optional some-optional-arg). There may be no required
arguments at all and only optional ones. The last encountered speciality of
the lambda-list is the &rest keyword, which signifies that everything else
after this is interpreted as one argument. For example (&rest message) means
that you can supply an arbitrary amount of things and they will all be used
as a message.

If the lambda-lists are still unclear to you, I'm sure you'll get it once
you start looking at actual commands. Commands will be listed according to
the module they are provided by. Note that not all commands are listed.

### Essentials
This module provides a couple of commands that are useful to the general user,
but mainly concerns itself with bot management. The most important command in
this list will be HELP as it allows you to retrieve information on other
commands.

* ECHO (&rest args)
  Returns whatever you send after the echo as a message, producing an echo.
  Example: `!echo my message`

* TIME ()
  Returns the current bot-local time.

* HELP (&rest command-signature)
  Attempts to display help on the command that most closely matches whatever
  signature or name you supplied. Do not include the prefix in the signature.
  Example: `!help notify`

* APROPOS (&rest command-signature)
  The same as HELP but displays information on all matches.
  Example: `!apropos notify`

* VERSION ()
  Shows version information of the Colleen instance you're communicating with.

* UPTIME ()
  Tells you how long this bot instance has been running for.

* LAST-SEEN (nick)
  Tells you if, and if how long ago, the nick has last been seen to be active.

### Convert
Allows various conversions between units and other things.

* CONVERT-TO METRIC (unit amount)
  Convert from imperial to metric units.
  Example: `!convert-to metric mile 12

* CONVERT-TO IMPERIAL (unit amount)
  Convert from metric to imperial units.
  Example: `!convert-to impreial c 32

* CONVERT-TO TINY (&rest message)
  Makes the message use tiny unicode characters where possible.

* CONVERT-TO FULLWIDTH (&rest message)
  Converts to fullwidth unicode characters.

* CONVERT-TO MD5 (&rest text)
  Computes an MD5 hash of the text.

* CONVERT-TO UNICODE (codepoint)
  Convert from decimal or hex (U+) to the actual unicode character.

### Counter
This module provides the ability to count for certain expressions within messages.
As such it is triggered automatically if you write something that contains a string
that should be counted. If this happens, Colleen will note the current status of the
counter.

One such counter in #Stevenchan is 'same'. Simply type a message containing the word
'same' and it should show the count. (The counter is a joke on the fact that 'same'
in Japanese (さめ) means shark.

### Dictionary
A simple dictionary that allows you to save information on a term within the bot and
retrieve this information later. Looking up a term can be done by either invoking
DICTIONARY ABOUT (eg `!dictionary about irc`) or by speaking to the bot directly
(eg `Colleen: irc`).

* DICTIONARY DEFINE (&rest definition)
  Defines a new dictionary term. You need to separate the term and the definition
  itself with a colon. If you want a term to simply be a link (redirect) to another,
  write the definition as in the second example.
  Example: `!dictionary define example: A way to illustrate a point`
  Example: `!dictionary define eg: => example`

* DICTIONARY REMOVE (&rest term)
  Removes the term from the dictionary.

* DICTIONARY SEARCH (&rest term)
  Searches the dictionary for terms that match the given term.

### Emoticon
Provides simple 'emoticons'. These are merely special strings that when encountered
alone in a message will provoke the bot to print a specific reply. All emoticons are
surrounded by colons. An example would be `:scream:`.

* EMOTICON ADD (name &rest emoticon)
  Defines an emoticon with the given name and emoticon as reply text. If the name is
  not already surrounded by colons, they are added automatically.
  Example: `!emoticon add pepsi http://shinmera.tymoon.eu/public/pepsi.gif

* EMOTICON REMOVE (name)
  Removes the specified emoticon.

* EMOTICON LIST ()
  Lists all defined emoticons.

### Google
This module allows interaction with a variety of google services.

* GOOGLE TRANSLATE (&rest text)
  Attempts to auto-detect the language of text and translate it to english.
  Example: `!google translate お尻

* GOOGLE GEOCODE (&rest address)
  Retrieves geocoding information about the given address.

* GOOGLE TIME (&rest address)
  Attempts to find time information about the address.

### Markov
This module records everything that is said on the channel and adds it into its
[Markov Chain](http://en.wikipedia.org/wiki/Markov_chain). Occasionally it will
reply with a message built out of this chain.

* MARKOV SAY ()
  Constructs a random message computed by the markov chains.

### Medals
Maintains a very crude medals system that allows bot operators to award other people
with arbitrary medals.

* MEDALS SHOW (&optional user)
  Shows the medals of the named user. The username defaults to whomever invoked the
  command.

### Notify
Allows you to write notifications to other users or for a specific time. This is mostly
useful for the times when someone isn't around and you'd like to remind them of
something once they return.

* NOTIFY (recipient &rest message)
  Notifies the recipient with the message whenever they next talk in the same channel.
  Example: `!notify shinmera Hey I'm testing Colleen!`

* NOTIFY @JOIN (recipient &rest message)
  Same as the regular notify except that the message is displayed whenever the user
  next joins the channel.

* NOTIFY @IN (time &rest message)
  Shows the notification message after time has passed. The time should be in the
  form of h:m:s, wherein the seconds are optional.
  Example: `!notify @in 0:0:5 Hi!`

* NOTIFY @DATE (date &rest message)
  Shows the message on the specified date. The date should be specified like so:
  2014.07.15T18:44:0

### Search
This module allows you to perform search queries on a variety of sites, most
prominently google and wikipedia. All the search lookups with the exception of a
few work the exact same so I won't elaborate further on them.

* SEARCH GOOGLE
* SEARCH WIKIPEDIA
* SEARCH WIKIPEDIA-DE
* SEARCH WIKTIONARY
* SEARCH ED
* SEARCH TOUHOU
* SEARCH CLIKI
* SEARCH CLHS
* SEARCH STEAM

### Shiritori
Allows you to play a game of [Shiritori](http://en.wikipedia.org/wiki/Shiritori)
in the current channel. The module will automatically attempt to convert romaji
into hiragana so that you don't need to activate a special input mode to play.

* SHIRITORI START ()
  Starts a new game of shiritori.

* SHIRITORI STOP ()
  Stops a game in progress.

* SHIROTIRI RULES ()
  Lists a brief summary of the rules of Shiritori.

### Silly
Since bots should also allow for some fun, this module is mostly for automatic
replies to certain messages or for commands that otherwise have no purpose
aside from amusement. I will not elaborate any of the automatic triggers as
to not ruin the magic.

* THANKS ()
  Thanks you.

* JERKCITY ()
  Replies with a random [jerkcity](http://jerkcity.com/) strip.
    
* EXPAND (acronym)
  Attempts to expand the acronym into words using random matching words
  from the Urban Dictionary popular word list. Results may vary.
  Example: `!expand IRC`

### Stevenchan
Provides integration with the Stevenchan image board and related events.

* STEVENCHAN LATEST ()
  Shows the latest poast on the board.

* STEVENCHAN MOVIE ()
  Shows which movie is most likely going to be next on the Stevenchan
  Movie Night as well as time information and a link to the stream.

### Urlinfo
Urlinfo will reply with information about an URL whenever one is posted
in a channel it is active in. The information includes the mime type on
non-HTML links and the title on HTML pages. It will also reply with the
'true' URL if the pasted URL contains redirects.

* URL ABOUT (url)
  Provides the same as simply pasting the URL into the activated
  channel. May still be useful in private messages or in channels
  where urlinfo is not activated.

### Weather
Provides weather information using the google geocoding API to resolve
an address and the weather.io API to retrieve weather information.

* WEATHER (&rest location)
  Attempts to resolve the location and display current weather
  information about it. This may or may not be inaccurate due to
  resolving problems or weather data inaccuracies.
  Example: `!weather New York`

### Welcome
Replies with a general welcoming reply or a personalised message once
a user joins a channel and utters a recognised greeting. 

* WELCOME SET (&rest message)
  Set the welcome replies that should be used for you personally.
  You can split off multiple by separating them with a vertical
  bar | .
  Example: `!welcome set Heyyy!|Oh, hi!`

* WELCOME ADD (&rest message)
  Add a new personalised welcome message to your list.

* WELCOME CLEAR ()
  Clears all of your personalised welcome messages again.


What Else
---------
There are a lot more commands and features that Colleen offers and
it has the potential to offer whatever you want provided someone
writes it. If you have suggestions or reports or maybe even fixes
and implementation patches, any kind of support to development
would be greatly appreciated.
