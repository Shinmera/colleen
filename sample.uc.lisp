
{EQUAL
 :SERVERS {EQUAL
           :DEFAULT {EQUAL
                     :HOST NIL,
                     :NICK "-Colleen",
                     :PORT 6667,
                     :REAL "-Made with secret alien technology",
                     :USER NIL,
                     :PASS NIL,
                     :CHANNELS (),
                     :NICKSERVPW NIL,
                     :PING-STEP 5,
                     :PING-WARN 30,
                     :PING-TIMEOUT 60,
                     :RECONNECT-COOLDOWN 5},
           :TYNET {EQUAL
                   :HOST "-irc.tymoon.eu",
                   :CHANNELS ()},
           :FREENODE {EQUAL
                      :HOST "-irc.freenode.org",
                      :CHANNELS ()}},
 :MESSAGES {EQUAL
            :QUIT "-See you, space cowboy...",
            :JOIN NIL,
            :PART NIL,
            :NO-COMMAND "-I don't know what you mean, $NICK$.",
            :NOT-AUTHORIZED "-Sorry, $NICK$. I can't let you do that.",
            :AUTH "-Welcome back, $NICK$!",
            :AUTH-WAIT "-Authentifying you, please wait...",
            :AUTH-FAIL "-I'm sorry, I don't recognize you.",
            :AUTH-OUT "-See you soon, $NICK$!",
            :AUTH-ALREADY "-$NICK$, I still remember you.",
            :MARKOV-NOTHING "-I have nothing to say."},
 :COMMAND {EQUAL
           :PREFIX ("-./" "-!")},
 :STARTUP {EQUAL
           :MODULES ("-auth" "-essentials"),
           :SERVERS ()}} 
