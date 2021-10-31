;/* *********************************************************************
;Function Name: mexican-train
;Purpose:  starting point of the game. Take user input on how to start the game and starts game.
;Parameters:    none

;Return Value:     startgame->function that creates or loads the game state.
;Algorithm:   read a valid user input and call startgame function with the valid input
;Assistance Received: none
;********************************************************************* */

(defun mexican-train()
    ( print "Please select one of the options")
    ( print "(1) to start a new game")
    ( print "(2) to load a saved game from a file")
    (LET ((useroption ( readnum 1 2)))  
        ;sends the user option to start game function which defines how the game is to  be played
        (startgame useroption (list 0 0) 1)
    )
)

;/* *********************************************
;Source Code to get input from the user and do input validation
;********************************************* */

;/* *********************************************************************
;Function Name: readnum
;Purpose: read a value and call validation
;Parameters:    1)start--> starting range
;               2)end -->end range of input 
;Return Value:  validate funuction
;Algorithm:   none
;Assistance Received: none
;********************************************************************* */

(defun readnum (start end)
    (print "Enter value in the range:")
    (princ start)
    (princ "-")
    (princ end)
    (TERPRI)
    (LET ((userInput (READ)))
    (validate userInput start end ))  )


;/* *********************************************************************
;Function Name: validate
;Purpose: to validate a given number num in the start and end range.
;Parameters:    1) num---> number to be validated
;               2)start -->start range of the number
;               3)end---> end range of the number 
;Return Value:  num if valid number , readnumber if invalid number.
;Algorithm:   Read the number if not valid type or not in range
;             call readnum to get another number else return the number.
;Assistance Received: none
;********************************************************************* */

(defun validate (num start end)
    (cond 
        ( (NOT (NUMBERP num))
			(print "Data type not valid! Enter integer in range ") 
            (readnum start end)  )
        ( ( < num start )
            (print "Invalid! value out of range") 
            (readnum start end)  )
        ( ( > num end )
            (print "Invalid! value out of range") 
            (readnum start end)  )
        ( t num )) )

;;this function helps to get a valid train input from the user
;/* *********************************************************************
;Function Name: readtrain
;Purpose: to read a valid character that represents train
;Parameters:    none    
;Return Value:  validtrain function that validates the input character
;Algorithm:   none
;Assistance Received: none
;********************************************************************* */
(defun readtrain ()
    (LET* ((userInput (READ)))
    (validtrain userInput ))  )
    

;/* *********************************************************************
;Function Name: validtrain
;Purpose:   to check the validity of user input entered
;Parameters:    userInput
;Return Value:  userInput if the input is one of six values expeceted
;               read train function otherwise.
;Algorithm:     1)check if the input value is one of six options and return value
;               2) If the value doesnot match call the readtrain function to get new input. 
;Assistance Received: none
;********************************************************************* */
(defun validtrain (userInput)
    (cond 
        (  (NUMBERP userInput)
			(print "Data type not valid! Enter a character related to the train") 
            (readtrain)
        )
        ((or (or (or (or (or (string-equal userInput "M") (string-equal userInput "C")) (string-equal userInput "H")) (string-equal userInput "B")(string-equal userInput "S")) ) 
            (string-equal userInput "A")
            )    
            userInput)
        (t 
            (print "Invalid train entered! Choose among Human(H) computer(C) Mexican(M) Boneyard(B) Help(A) Serialize(S) ")
            (print ">>")
            (readtrain))
    )
)

;/* *********************************************************************
;Function Name: readcomputeroptions
;Purpose:  read the input for computer move
;Parameters:    none    
;Return Value:  validcomputerinput function that validates the input
;Algorithm:    none
;Assistance Received: none
;********************************************************************* */
(defun readcomputeroptions ()
    (LET* 
        (
            (userInput (READ))
        )
    (validcomputerinput userInput )) )


;/* *********************************************************************
;Function Name: validcomputerinput
;Purpose: to check if the computerturn input is valid
;Parameters:    userInput
;Return Value:  read computeroptions if the userInput is not valid
;               userInput if the value is "C" or "S"

;Algorithm:   check if the userInput is one of expected values and return it
;               if not call readcomputeroptions to get new input.
;Assistance Received: none
;********************************************************************* */
(defun validcomputerinput (userInput)
    (cond 
        (  (NUMBERP userInput)
			(print "Data type not valid! Enter a character related to the train") 
            (readcomputeroptions)
        )
        ((or  (string-equal userInput "C")(string-equal userInput "S")) 
             userInput
        )    
        (t 
            (print "Invalid Option chosen! Press C to continue and S to serialize and quit")
            (print ">>")
            (readcomputeroptions))
    )
)
;/* *********************************************************************
;Function Name: nextstep
;Purpose: to get input on the next step of the game
;Parameters:    none
;Return Value:  validnextstep function that checks if the input is valid or not.
;Algorithm: none    
;Assistance Received: none
;********************************************************************* */
(defun nextstep() 
    (print "Press Y to play new game and N to quit")
    (LET* ((userInput (READ)))
        (validnextstep userInput )
    )
)

;/* *********************************************************************
;Function Name: validnextstep
;Purpose: to check if the user input on the next step of the game is valid and return value
;Parameters:    userInput
;Return Value:  userInput if value is valid, nextstep function otherwise.
;Algorithm:  check if the value is one of two expected values, if yes return value
;           if not call nextstep function to get userInput again.
;Assistance Received: none
;********************************************************************* */
(defun validnextstep (userInput)
    (cond 
        (  (NUMBERP userInput)
			(print "Data type not valid!") 
            (nextstep)
        )
        ((or (string-equal userInput "Y") (string-equal userInput "N"))    
            userInput)
        (t 
            (print "Invalid input entered! ")
            (print ">>")
            (nextstep))
    )
)
;-----------------------------------------------------------------------

;/* *********************************************
;Source Code to load/start game and play rounds start from here.
;********************************************* */

;/* *********************************************************************
;Function Name: startgame
;Purpose: to initialize a new game based on the user input and continue the game as long as player wants.
;Parameters:    1)useroption ---> user input value that defines whether player wants to load a saved file or start a fresh game
;               2)gamescores ---> current scores of the game for two player, which is empty(0 0) when first called.
;               3)round ---> current round of the game
;
;Return Value:  if game is over prints winner of the game. calls startgame if the player wants to continue to next game
;Algorithm:     1)initialize deck from scratch using random number generation for a fresh game or load file contents for a saved game
;               2)calls playround function to start playing the moves for the round until the game is over and gets player scores
;                   as return value
;               3)check the player scores to find if the game was serialized or noti.e.(0 0) score if the round was serialized.
;               4)ask user input on if the user wants to continue next round
;               5)use the userinput to decide if continue playing games or display the winner and quit.
;   
;Assistance Received: none
;********************************************************************* */
(defun startgame (useroption gamescores round)
    (cond 
       ( (= useroption 1)
            (print "User decided to start a new game") 
            (print "----------------------------------------------------------------------------------")
            
            ;generating a deck of 55 tiles and shuffling it
            (let* ( (deck (shuffledeck (generatedeck 9 9 )))
                    (enginetile (GetengineTile round))
                    (enginetile_pos (getTilelocation 54 deck (car enginetile)))
                    (finaldeck ( removeTile (+ enginetile_pos 1) deck))
                    (humantiles (sublist finaldeck 0 16)) 
                    (computertiles (sublist finaldeck 16 16))
                    (boneyardtiles (sublist finaldeck 32 nil))
                    ;playround function is called here which starts a move for a game and runs recursively until round is over.
                    (playerscores (playround humantiles computertiles boneyardtiles enginetile enginetile enginetile  round
                        (setnextplayer gamescores) (list nil nil 0) gamescores))
                    (humantotalscore (+ (first playerscores) (first gamescores)))
                    (computertotalscore (+ (second playerscores) (second gamescores)))
                    (scoreslist (cons humantotalscore (cons computertotalscore (list ()))))
                )
                ;next steps are taken based on player scores and user input
                (cond ((and (= (first playerscores) 0) (= (second playerscores) 0  ) )
                        (print "The game has been serialized thank you!")
                    )
                    (T
                        (print "The round has ended") 
                        (print "Game score of humanplayer is: ")
                        (princ  (+ (first playerscores) (first gamescores)))
                        (print "Game score of computerplayer is: ") 
                        (princ (+ (second playerscores) (second gamescores)))
                        (let* 
                            ((nextstep_input (nextstep)))
                            (cond
                                ;player wants to play one more round.
                                ((string-equal nextstep_input "Y")
                                    (startgame 1 scoreslist (+ round 1))  
                                )
                                (T
                                    (cond ((> humantotalscore computertotalscore)
                                            (print "Human player is the winner!")
                                        )
                                        (t 
                                            (print "Computer Player is the winner")
                                        )    
                                    )
                                )))))))
       ( (= useroption 2)
            ;need to read a file here and start a game from here
            (print "User decided to load a saved game") 
            (let*
            (   ;loadsaved file contains the content of the file in list format.
                (file (loadsavedfile))
                (round  (first file))
                (computerscore (second file))
                (computerhand (second (cdr file)))
                (computertrain (second (cdr (cdr file))))
                (humanscore (second (cdr (cdr (cdr file)))))
                (humanhand (second (cdr (cdr (cdr (cdr file))))))
                (humantrain (second (cdr (cdr (cdr (cdr (cdr file)))))))
                (mexicantrain (second (cdr (cdr (cdr (cdr (cdr (cdr file))))))))
                (boneyardtiles (second (cdr (cdr (cdr (cdr (cdr (cdr (cdr file)))))))))
                (nextplayer (second (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr file))))))))))
                (humannext (gethumannext nextplayer))
                (filteredcomputertrain (removecomputermarker computertrain))
                (initialreverse (reverse filteredcomputertrain))
                (reversecomputertrain (reversecomputertrain initialreverse (list )) )
                (filteredhumantrain (removehumanmarker humantrain))
                (computertrainmark  (computertrainmarker computertrain))
                (humantrainmark (humantrainmarker humantrain))
                (markerslist (cons humantrainmark (cons computertrainmark (cons 0 ()))))
                (gamescores (cons humanscore (cons computerscore ())))
                ;playround function is called here which starts the round moves and calls recursively until the game is over.
                (playerscores (playround humanhand computerhand boneyardtiles filteredhumantrain reversecomputertrain 
                    mexicantrain round humannext markerslist gamescores))
                (humantotalscore (+ (first playerscores) (first gamescores)))
                (computertotalscore (+ (second playerscores) (second gamescores)))
                (scoreslist (cons humantotalscore (cons computertotalscore (list ()))))
            )
            ;;--------------------------expressions below this------------------------------------------------------------
                (cond ((and (= (first playerscores) 0) (= (second playerscores) 0 ) )
                        (print "The game has been serialized thank you!")
                        t
                    )
                    ;since the score is not( 0 0) game was not serialized
                    (T
                        (print "The round has ended!!") 
                        (print "Game score of humanplayer is: ")
                        (princ  (+ (first playerscores) (first gamescores)))
                        (print "Game score of computerplayer is: ") 
                        (princ (+ (second playerscores) (second gamescores)))
                        (let* 
                            (   ;user input on whether user wants to play next round.
                                (nextstep_input (nextstep))
                            )
                            (cond
                                ((string-equal nextstep_input "Y")
                                    ;start another round
                                    (startgame 1 scoreslist (+ round 1))   
                                )
                                (T
                                    (cond ((> humantotalscore computertotalscore)
                                            (print "Human player is the winner!")
                                        )
                                        (t 
                                            (print "Computer Player is the winner")
                                        )    
                                    )
                                
                                ))))))
                            )
       (t (print  "Invalid input please restart the game"))
))


;/* *********************************************************************
;Function Name: playround
;Purpose: to recursively perform the moves of human and computer player until the game is over.
;Parameters:    1)humantiles -->list of tiles of human player
;               2)computertiles -->list of tiles of computer player
;               3)boneyardtiles --> list of tiles allocated to boneyard
;               4)humantrain --> list that contains tiles of human train 
;               5)computertrain --> list that contains tiles of computer train
;               6)mexicantrain --> list that contains tiles of mexicantrain
;               7)roundnumber -->current round number of the game
;               8)humannext -->variable that denotes if human player's turn is next stores T or nil
;               9)markerslist --> computer and human train marked status in list along with continous turns played 
;                       variable in format (marker marker continousturns) or (nil nil 0)
;
;Return Value: if the round is over returns returnscore function that returns the score of players
;                   else returns other functions that eventually call playround function. 
;
;Algorithm:     1)Display game status
;               2)Check if the game is over or not if so return the scores back 
;               3)if game is not over check human next value to find the next player and take input based on player
;               4)call the functions based on input in order to allow the players to make move and play game.
; 
;Assistance Received: none
;********************************************************************* */
(defun playround(humantiles computertiles boneyardtiles humantrain computertrain mexicantrain roundnumber humannext markerslist gamescores) 
    
    ;this helps to display the changes each time move is made from a player.
    ( display humantiles computertiles boneyardtiles humantrain computertrain mexicantrain roundnumber (GetengineTile roundnumber) humannext markerslist gamescores)
    (cond 
        ((equal (length humantiles) 0)
            (Print "User played all the tiles so the game is over")
            (returnscores 0  (totalscoreoftrain computertiles 0))   
        )
        ((equal (length computertiles) 0)
            (Print "Computer played all the tiles so the game is over")
            (returnscores (totalscoreoftrain humantiles 0) 0 )
        )
        (  (and (and  (equal (Computertrainmarked markerslist ) t) (equal (Humantrainmarked markerslist ) t))   (equal (length boneyardtiles) 0)   )
            (print "boneyard is empty and both players train is marked so game is over")
            (returnscores (totalscoreoftrain humantiles 0) (totalscoreoftrain computertiles 0))
        )
        (t 
            (cond
                ((eq humannext t)
                    (Displaynextplayer "Human")
                    (LET ( ;character input for the train that the user wants to place the tile to.
                            (traininput (readtrain))  
                        )
                        ;----------------------------------expression below use the variable above-----------------
                        (cond ((string-equal traininput "B")
                                ;(print "User picked up a tile from the boneyard tile added to the list.")
                                ;function to add tiles to user train goes here.
                                (boneyardtoplayertiles humantiles computertiles boneyardtiles humantrain computertrain mexicantrain roundnumber (GetengineTile roundnumber) nil markerslist gamescores)
                            )
                            ((string-equal traininput "S")
                                ;user should serialize and quit
                                (SerializeandQuit humantiles computertiles boneyardtiles humantrain computertrain mexicantrain roundnumber humannext markerslist gamescores)
                                (print "Human player serialized and quit")
                                (list 0 0)
                            )
                            ; human player is asking for assistance.
                            ((string-equal traininput "A")
                                (GetComputerhelp humantiles computertiles boneyardtiles humantrain computertrain mexicantrain roundnumber humannext markerslist gamescores)
                                (playround humantiles computertiles boneyardtiles humantrain computertrain mexicantrain roundnumber humannext markerslist gamescores)
                            )
                            (t 
                                ;if none of the options user wants to play one of the trains.
                                (let* (
                                    (tileinput (readnum 1 (length humantiles)))
                                    ;tile is the actual tile(x y) that user chose to play to the desired train
                                    (tile (sublist humantiles (- tileinput 1) 1 ))
                                    ;train to play has the tiles of the train that user wants to place the tile to 
                                    (traintoplay (Gettrain traininput humantrain computertrain mexicantrain ))
                                    ;trainisplayable holds if the user train is playable for the tileinput
                                    (trainisplayable (checktrain traintoplay tile))
                                    ;if orphan train exists
                                    (orphanexists (Orphandoubleexists humantrain computertrain mexicantrain markerslist))
                                    ;was orphan train played in case it existed.
                                    (orphanplayed (Isorphantrain traintoplay)))
                                    ;;--------------variables above---------------------------
                                    
                                    ;CheckOrphanandPlay is responsible for checking if orphan train exists before letting player to play any trains.
                                    (CheckOrphanandPlay trainisplayable tileinput traintoplay tile humantiles computertiles boneyardtiles
                                    humantrain computertrain mexicantrain roundnumber (GetengineTile roundnumber) traininput nil markerslist orphanexists orphanplayed gamescores)
                                    ;-------------------------end of expressions----------------------------------------------------

                                )))))
                (t
                    (Displaynextplayer "Computer")
                    (LET (  ;character input for the train that the computer wants to place the tile to.
                            ;this should read options to play or serialize rather than getting other options.
                            (traininput (readcomputeroptions))  
                        )
                        ;----------------------------------expression below use the variable above-----------------
                        (cond 
                            ((string-equal traininput "S")
                                ;user should serialize and quit
                                (SerializeandQuit humantiles computertiles boneyardtiles humantrain computertrain mexicantrain roundnumber humannext markerslist gamescores)
                                (print "Computer player serialized and quit")
                                ;returning an empty list incase game is serialized.This allows caller function to know game was serialized.
                                (list 0 0)
                            )
                            (t  
                                ;else use computer strategy to play the next move
                                (Computerplayerstrategy humantiles computertiles boneyardtiles humantrain computertrain mexicantrain roundnumber t markerslist gamescores)
                            )
                        )))))))




;/* *********************************************
;Source Code of the helper functions for startround function that intializes the game.
;********************************************* */

;;;this needs to have  atomic input list output as we need a new list
;/* *********************************************************************
;Function Name: generatedeck
;Purpose: generate a deck of 55 mexican train tiles 
;Parameters:    startvalue endvalue
;Return Value:  list of 55 tiles in order
;
;Algorithm:     1) Create tile starting from (9 9) (9 8) until (9 0) is reached where last value is 0
;               2) Decrease the first value and last value by 1 and do this recursively until last value is 0 again.
;               3) Do this until both first and last value are -1
;               4) Use cons to add tiles to each other and form a list of 55 tiles.
;Assistance Received: none
;********************************************************************* */
(defun generatedeck (startvalue endvalue)
    (cond ((and (= startvalue -1)  ( = endvalue -1) )
            () )
        ((= endvalue 0)  
            (LET ((templist (cons startvalue( cons endvalue ()) ))) 
               (cons templist (generatedeck (- startvalue 1) (- startvalue 1)))
            )      
        )
        (t  
            (LET ((templist (cons startvalue( cons endvalue ()) ))) 
                (cons templist (generatedeck startvalue (- endvalue 1)))
            ) 
        )))


;;function shuffle deck helps to shuffle the deck we have created to randomly organize the tiles in a deck
;/* *********************************************************************
;Function Name: shuffledeck
;Purpose:  to shuffle a deck of 55 tiles in a random order
;Parameters:    deck --> that contains a list of 55 tiles
;Return Value:  shuffled deck
;Algorithm:     1)use random number generator to get a tile from a deck 
;               2) use elt function to access the random tile and use cons to add tiles
;               3) use removeatile function to remove the tile from the deck so it doesnot repeat
;               4)  call shuffle deck again and follow until deck is empty
;Assistance Received: none
;********************************************************************* */
(DEFUN shuffledeck(deck)
    (cond
        ( (null deck)
            ()  
        )
        (t
            (LET*(
            ;
                (randomize (make-random-state t))
                (tile_position (random (length deck) randomize)))
                (cons
                ;ELT function accesses specified elements of sequences. The index is counted from zero. 
                    (elt deck tile_position)
                    (shuffledeck (removeTile(+ tile_position 1) deck))
                )
            )
        )
    )
)

 
;/* *********************************************************************
;Function Name: sublist
;Purpose: get the part of the list.
;Parameters: 
;                1) vector -->list of the tiles
;                2) idx --> index of first tile
;                3) number of tiles to get
;Return Value:  list of tiles in given range
;Algorithm:   get the len number of tiles from idx value from the vector list and return it.
;Assistance Received: http://www.lee-mac.com/sublist.html
;********************************************************************* */
(defun sublist ( vector idx len )
    (cond
        (   (null vector) nil)
        (   (< 0  idx) (sublist (cdr vector) (- idx 1) len))
        (   (null len) vector)
        (   (< 0  len) (cons (car vector) (sublist (cdr vector) idx (- len 1))))
    )
)


;/* *********************************************
;Source Code for helper functions for playing round.
;********************************************* */


;/* *********************************************************************
;Function Name: setnextplayer
;Purpose: set the player to go first based on the current score of the game
;Parameters:    gamescore that stores the score of the players
;Return Value:  t if human goes next or nil if computer goes next
;Algorithm:     1)if humanscore<computerscore humangoes first
;                2)if computerscore<humanscore human goes first
;                3)else toss decides who goes next
;Assistance Received: none
;********************************************************************* */
(defun setnextplayer(gamescores)
    (cond
        ((> (first gamescores) (second gamescores))
            (print "Since computer has lower game score computuer goes next")
            nil
        )
        ((< (first gamescores) (second gamescores))
            (print "Since human has lower game score human goes next")
            t
        )
        (T
            (print "TOSS for player to go first:")
            (print "Press 0 for heads 1 for tails")
            (let* 
                    (
                        (inputnum (readnum 0 1))
                        (randomize (make-random-state t))
                        (randomnum (random 2 randomize))
                        
                    )  
                (print "Input value:")    
                (princ inputnum)
                (print "Random toss value:")
                (princ randomnum)    
                (cond
                    ((= randomnum inputnum)
                        (print "You won the toss start the game")
                        t
                    )
                    (T
                        (print "computer won the toss. Computer goes first")  
                        nil
                    )
                
                )))))


;/* *********************************************
;Source Code for display functions that help to display the output to the user.
;********************************************* */

;/* *********************************************************************
;Function Name: Printscoresforround
;Purpose: print currentscores of the round.
;Parameters:    humanscore and computerscore
;Return Value:  
;Algorithm:   none
;Assistance Received: none
;********************************************************************* */
(defun Printscoresforround(humanscore computerscore)

    (print "Human score for the round:  " )
    (princ humanscore)
    (print "Computerscore for the round  : ")
    (princ computerscore)
)
;/* *********************************************************************
;Function Name: Displayhelpmsg
;Purpose: Displays the help message for human player
;Parameters:    train -->train to play
;               tile --> tile to play
;               reason --> reason to play that tile
;Return Value:  
;Algorithm:   none
;Assistance Received: none
;********************************************************************* */
(defun Displayhelpmsg (train tile reason)

    (print "Play tile:")
    (princ tile)
    (princ " to " )
    (princ train)
    (princ "train as")
    (princ reason)

)
;/* *********************************************************************
;Function Name: DisplayMaxtile
;Purpose: This function displays the largest tile possible to play as the help for human player.
;Parameters:    1)humantiles -->list of tiles of human player
;               2)computertiles -->list of tiles of computer player
;               3)boneyardtiles --> list of tiles allocated to boneyard
;               4)humantrain --> list that contains tiles of human train 
;               5)computertrain --> list that contains tiles of computer train
;               6)mexicantrain --> list that contains tiles of mexicantrain
;               7)roundnumber -->current round number of the game
;               8)humannext -->variable that denotes if human player's turn is next stores T or nil
;               9)markerslist --> computer and human train marked status in list along with continous turns played 
;                       variable in format (marker marker continousturns) or (nil nil 0)
;               10)gamescores --> game scores of the players
;Return Value:  
;Algorithm:  Get the largest possible tile to play as well as the train to play
;Assistance Received: none
;********************************************************************* */

(defun DisplayMaxtile(humantiles computertiles boneyardtiles humantrain computertrain mexicantrain roundnumber humannext markerslist gamescores)

    (let* 
        (
            (Htrainplayable  (playabletileexists humantiles humantrain))
            (Ctrainplayable  (playabletileexists humantiles computertrain))
            (Mtrainplayable  (playabletileexists humantiles mexicantrain))
            (maxtileHtrain   ( getplayablemax humantiles humantrain (list -1 -1)))
            (maxtileCtrain   ( getplayablemax humantiles computertrain (list -1 -1)))
            (maxtileMtrain   ( getplayablemax humantiles mexicantrain (list -1 -1)))
            (Ctrainmarked  (Computertrainmarked markerslist) )
            (maxtrain (first  ( getmaxTiletrainhelp maxtileCtrain maxtileHtrain maxtileMtrain Ctrainmarked) ))
            (tiletoplay (second (getmaxTiletrainhelp maxtileCtrain maxtileHtrain maxtileMtrain Ctrainmarked)))
        )
        
        (cond 
            ((string-equal maxtrain "H")
                (Displayhelpmsg "Human" tiletoplay  " it is the largest tile possible" )
            )
            ((string-equal maxtrain "C")
                (Displayhelpmsg "Computer" tiletoplay  " it is the largest tile possible" )
            )
            ((string-equal maxtrain "M")
                (Displayhelpmsg "Mexican" tiletoplay  " it is the largest tile possible" )
            )
        )
    )

)
;/* *********************************************************************
;Function Name: displayforceorphanmsg
;Purpose: This function displays this message when orphan double train is created.
;Parameters: tileplayed and  train played
;Return Value:  -
;Algorithm:  none 
;Assistance Received: none
;********************************************************************* */

(Defun displayforceorphanmsg(tile train)
    (print "Computer player plays the tile: ")
    (princ tile)
    (princ " to the: ")
    (princ train)
    (princ "train as it forces opponent to play orphan double train")

)
;/* *********************************************************************
;Function Name: Displaynextplayer
;Purpose: This function displays the next player to play and asks for valid input.
;Parameters: player to play next
;Return Value:  -
;Algorithm:  none 
;Assistance Received: none
;********************************************************************* */

( defun Displaynextplayer (player)
    
    (TERPRI)
    (princ player)
    (princ "'s turn next") 
    (cond
        ((string-equal player "computer")
            (print "Please enter (C) to continue and (S) to serialize and quit.")
        )
        (T
         (print "Please enter as per your choice:(play-HTrain(H) , PlayCTrain(C) ,PlayMTrain(M) ,Pick-Boneyard(B),Serialize(S) or Assistance(A))")
        )
    )
    (terpri)
    (princ ">>")
) 
;/* *********************************************************************
;Function Name: display
;Purpose:  This function deals with displaying game state on the screen.
;Parameters:    1)humantiles -->list of tiles of human player
;               2)computertiles -->list of tiles of computer player
;               3)boneyardtiles --> list of tiles allocated to boneyard
;               4)humantrain --> list that contains tiles of human train 
;               5)computertrain --> list that contains tiles of computer train
;               6)mexicantrain --> list that contains tiles of mexicantrain
;               7)roundnumber -->current round number of the game
;               8 engine tile --> engine tile for current round
;               9)humannext -->variable that denotes if human player's turn is next stores T or nil
;               10)markerslist --> computer and human train marked status in list along with continous turns played 
;                       variable in format (marker marker continousturns) or (nil nil 0)
;               11)gamescores --> game scores of the players
;Return Value:  --
;Algorithm:   Allocate 3 lines for each tile so that vetical tiles can be displayed as needed.
;Assistance Received: none
;********************************************************************* */

(defun display(humantiles computertiles boneyardtiles humantrain computertrain mexicantrain roundnumber enginetile humannext markerslist gamescores)

    
    (print "-------------------------------------------------------------------------------------------------------------------------------------------------")
    (print "current round number")
    (princ roundnumber)
    (print "Human player score for the game: ")
    (princ (first gamescores))
    (princ " Computer player score for the game: ")
    (princ (second gamescores))
    (print "Human player score for the round: ")
    (princ (totalscoreoftrain humantiles 0))
    (princ " Computer player score for the round: ")
    (princ (totalscoreoftrain computertiles 0))
     
    (print "Human Tiles: ")
    (TERPRI)
    (displaytiles humantiles 1 )
    (TERPRI) (TERPRI) (TERPRI)
    
    ;;-----------------------------Human train-----------------------------------------------
    ;works on 3 line system to display double tile.
    ; displaydoubletraintile-->  8       "empty"
    ; displaytraintile--->       |    or  2-6  
    ; displaydoubletraintile-->  8       "empty" 
    (cond
        ;if less than 15 tiles print all of them in one line
        ((<= (length humantrain) 15)
                (format T "~80@a" "")
                (displaydoubletraintiles (rest humantrain) 1)
                (TERPRI)
                (format T "~80@a" "Humantrain-->")
                (displaytraintiles (rest humantrain) 1)
                (displayhumanmarker markerslist )
                (TERPRI)
                (format T "~80@a" "")
                (displaydoubletraintiles (rest humantrain) 1)
                (TERPRI)
        
        )
        ;if more than 15 tiles print them in 2 lines so that vertical tiles can be displayed properly.
        (T
                ;first 15 tiles display
                (format T "~80@a" "Humantrain-->")
                (displaydoubletraintiles (sublist humantrain 1 15) 1)
                (TERPRI)
                (format T "~80@a" "")
                (displaytraintiles (sublist humantrain 1 15) 1)
                (TERPRI)
                (format T "~80@a" "")
                (displaydoubletraintiles (sublist humantrain 1 15) 1)
                (TERPRI)
                ;-----remaining part of the tiles displayed
                (displaydoubletraintiles (sublist humantrain 16 nil) 1)
                (TERPRI)
                (displaytraintiles (sublist humantrain 16 nil) 1)
                (TERPRI)
                ;marked displayed at the end of human 
                ( displayhumanmarker markerslist )
                (displaydoubletraintiles (sublist humantrain 16 nil) 1)
                (TERPRI)
        ))
    ;-----------------------Displaying engine tile---------------------------------------------------------------

    (format T "~80@a" "")
    (princ (first (first enginetile)))
    (TERPRI)
    (format T "~81@a" "EngineTile-->|")
    (TERPRI)
    (format T "~80@a" "")
    (princ (second (first enginetile)))
    (TERPRI)
    ;-----------------------------Displaying computer train--------------------------------------------------
    
    (printspaces (- 82 (* (length computertrain) 6) )) 
    (displaydoubletraintiles (reversecomputertrain (reverse (rest computertrain)) (list )) 1)
    (TERPRI)
    (printspaces (- 82 (* (length computertrain) 6) )) 
    (displaycomputermarker markerslist)
    (displaytraintiles (reversecomputertrain (reverse (rest computertrain)) (list )) 1)
    (princ " <- computer")
    (TERPRI)
    (printspaces (- 82 (* (length computertrain) 6) )) 
    (displaydoubletraintiles (reversecomputertrain (reverse (rest computertrain)) (list )) 1)
    (TERPRI)

    ;----------------------Displaying mexican train----------------------------------------------------------------------
    (cond
        ;display only 15 tiles on one line
        ((<= (length mexicantrain) 15)
                (format T "~80@a" "")
                (displaydoubletraintiles (rest mexicantrain) 1)
                (TERPRI)
                (format T "~80@a" "mexicantrain-->")
                (displaytraintiles (rest mexicantrain) 1)
                (TERPRI)
                (format T "~80@a" "")
                (displaydoubletraintiles (rest mexicantrain) 1)
                (TERPRI)
        )
        ;if more than 15 tiles create new line to display
        (T
                (format T "~80@a" "")
                (displaydoubletraintiles (sublist mexicantrain 1 15) 1)
                (TERPRI)
                (format T "~80@a" "mexicantrain-->")
                (displaytraintiles (sublist mexicantrain 1 15) 1)
                (TERPRI)
                (format T "~80@a" "")
                (displaydoubletraintiles (sublist mexicantrain 1 15) 1)
                (TERPRI)
                ;-----remaining part of the tiles
                (displaydoubletraintiles (sublist mexicantrain 16 nil) 1)
                (TERPRI)
                (displaytraintiles (sublist mexicantrain 16 nil) 1)
                (TERPRI)
                (displaydoubletraintiles (sublist mexicantrain 16 nil) 1)
                (TERPRI)
        ))
    ;---------------------------------------------------------------------------------
    (print "Computer Tiles: ")
    (TERPRI)
    (displaytiles computertiles 1)
    (TERPRI) (TERPRI) (TERPRI)
    (print "boneyard Tiles: ")
    (TERPRI)
    (displaytiles boneyardtiles 1)
    (print "--------------------------------------------------------------------------------------------------------------------------------------------------")
)


;/* *********************************************************************
;Function Name: displayhumanmarker
;Purpose: helps to display human marker if present
;Parameters: markerslist --> contains the list of markers
;Return Value:  -
;Algorithm:  none 
;Assistance Received: none
;********************************************************************* */
(defun displayhumanmarker( markerslist )
    (cond
        ((equal (Humantrainmarked markerslist) t)
            (princ "M")
        )
    )
)

;/* *********************************************************************
;Function Name: displaycomputermarker
;Purpose: ;helps to display computer marker if present
;Parameters:    markerslist
;Return Value:  --
;Algorithm:   none
;Assistance Received: none
;********************************************************************* */

(defun displaycomputermarker( markerslist )
    (cond
        ((equal (Computertrainmarked markerslist) t)
            (princ "M")
        )
    )
)

;/* *********************************************************************
;Function Name: printspaces
;Purpose: print the spaces given number of times
;Parameters:   times--> number of spaces needed
;Return Value:  ---
;Algorithm:   print a space and call printspaces function to print space "space-1" times
;Assistance Received: none
;********************************************************************* */
(defun printspaces(times) 
    (cond
        ((<= times 1)
            (princ " ")
        )
        (T
            (princ " ")
            (printspaces (- times 1))
        )))

;this function display the computer, human and boneyard tiles
;/* *********************************************************************
;Function Name: displaytiles
;Purpose: display the human , computer and boneyard pile tiles along with index number on the screen
;Parameters:    vector--> list of tiles
;Return Value:  number --> index of the next tile to be displayed
;Algorithm:   display the first tile, call displaytiles function with rest of tiles and also increase the index
;Assistance Received: none
;********************************************************************* */
(defun displaytiles (vector number)
    (cond 
        ((null vector)        )
        (t
            (princ " | ")
            (princ  number )
            (princ "->")
            (princ (car vector))
            (princ " | ")
            (displaytiles (cdr vector) (+ number 1)  )
            
        )
    )
)


;/* *********************************************************************
;Function Name: displaydoubletraintiles
;Purpose:   this function helps to display the top and bottom part of the vertical tile of a train.
;                   displaydoubletraintiles displays line1 ---> 2
;                         display tiles display ----->          |
;                   displaydoubletraintiles displays line3 ---> 2
;Parameters:    vector --> list of tiles
;               number  --> index of tile to be displayed.
;Return Value:  --
;Algorithm: none  
;Assistance Received: none
;********************************************************************* */
(defun displaydoubletraintiles (vector number)

    (cond 
        ((null vector))
        ((eq (first (car vector)) (second (car vector)))
            (princ "  ")
            (princ (first (car vector)))
            (princ "  ")
            (displaydoubletraintiles (cdr vector) (+ number 1)  )
        )
        (t
            (princ "     ")
            (displaydoubletraintiles (cdr vector) (+ number 1)  )
            
        )
    )
)


;/* *********************************************************************
;Function Name: displaytraintiles
;Purpose: display middle tile of a train tile among 3 lines of a tile.
;Parameters: vactor --> list of tiles
;           number --> index of tile to be displayed
;Return Value:  --
;Algorithm:   none
;Assistance Received: none
;********************************************************************* */
(defun displaytraintiles (vector number)

    (cond 
        ((null vector) )
        ((eq (first (car vector)) (second (car vector)))
            (princ "  |  ")
            (displaytraintiles (cdr vector) (+ number 1)  )
        )
        (t
            (princ "|")
            (princ (first (car vector)))
            (princ "-")
            (princ (second (car vector)))
            (princ "|")
            (displaytraintiles (cdr vector) (+ number 1)  )
            
        )
    )
)

;/* *********************************************************************
;Function Name: displaydoublemsg
;Purpose: display a message if a double tile was played.
;Parameters: tile --> tile which is checked for possible double tile.
;Return Value: -- 
;Algorithm:   check if a tile is double and display message.
;Assistance Received: none
;********************************************************************* */

(defun displaydoublemsg(tile)
    (cond
        ((eq (Checkdoubletile tile) t)
            (print "Since double tile was played one more play awarded to the same player.")
        )
    )  
)


;/* *********************************************
;Source Code for functions that deal with strategy of the game.
;********************************************* */


;/* *********************************************************************
;Function Name: updateplayertiles
;Purpose: this function updates the players tiles when a move is made and calls updateplayedtrain where train tiles are modified and then
;               playround function is called iteratively to make moves. 
;Parameters:    1)humantiles -->list of tiles of human player
;               2)computertiles -->list of tiles of computer player
;               3)boneyardtiles --> list of tiles allocated to boneyard
;               4)humantrain --> list that contains tiles of human train 
;               5)computertrain --> list that contains tiles of computer train
;               6)mexicantrain --> list that contains tiles of mexicantrain
;               7)roundnumber -->current round number of the game
;               8 engine tile --> engine tile for current round
;               9)humannext -->variable that denotes if human player's turn is next stores T or nil
;               10)markerslist --> computer and human train marked status in list along with continous turns played 
;                       variable in format (marker marker continousturns) or (nil nil 0)
;               11)gamescores --> game scores of the players
;               12)updatedplayertiles --> updatestiles of the current player
;               13) updatedplayedtrain --> updated train where the current player played
;               14)modified_humannext -->humannext based on the tile played.
;               15)traininput --> character value of the train played.
;               
;Return Value:  
;Algorithm:   
;Assistance Received: none
;********************************************************************* */
(defun updateplayertiles(updatedplayertiles updatedplayedtrain  humantiles computertiles boneyardtiles
                humantrain computertrain mexicantrain roundnumber enginetile traininput humannext markerslist modified_humannext gamescores ) 

    ;this condition should determine what trains value to change whether to change human tiles or the computer tiles for the next part.           
    (cond ((eq humannext nil)
            ;current player is human
            ;in this case humantiles will be replaced by updatedplayertiles
            (updateplayedtrain updatedplayedtrain updatedplayertiles computertiles boneyardtiles
               humantrain computertrain mexicantrain roundnumber enginetile traininput modified_humannext markerslist humannext gamescores)
        )
        (t 
            ;current player is computer
            ;in this case computer tile will be replaced by updatedplayertiles.
            (updateplayedtrain updatedplayedtrain humantiles updatedplayertiles boneyardtiles
              humantrain computertrain mexicantrain roundnumber enginetile traininput modified_humannext markerslist humannext gamescores)
        )      
    )
)

;/* *********************************************************************
;Function Name: updateplayedtrain
;Purpose: this function helps to update the tiles of the train where a tile was played
;Parameters:    1)humantiles -->list of tiles of human player
;               2)computertiles -->list of tiles of computer player
;               3)boneyardtiles --> list of tiles allocated to boneyard
;               4)humantrain --> list that contains tiles of human train 
;               5)computertrain --> list that contains tiles of computer train
;               6)mexicantrain --> list that contains tiles of mexicantrain
;               7)roundnumber -->current round number of the game
;               8 enginetile --> engine tile for current round
;               9)humannext -->variable that denotes if human player's turn is next stores T or nil
;               10)markerslist --> computer and human train marked status in list along with continous turns played 
;                       variable in format (marker marker continousturns) or (nil nil 0)
;               11)gamescores --> game scores of the players
;               12)updatedplayertiles --> updatestiles of the current player
;               13) updatedtrain --> updated train where the current player played
;               14)modified_humannext -->humannext based on the tile played.
;               15)traininput --> character value of the train played.
;
;Return Value: playround function is returned with modified value 
;Algorithm:   check the train played and return the playround function with parameters as needed.
;Assistance Received: none
;********************************************************************* */
(defun updateplayedtrain(updatedtrain humantiles computertiles boneyardtiles
                humantrain computertrain mexicantrain roundnumber enginetile traininput modified_humannext markerslist humannext gamescores)

    (cond ((string-equal traininput "M")
            ;train to be updated is mexican
            ;this continues the current round by modifying the trains values
            ( playround humantiles computertiles boneyardtiles humantrain computertrain updatedtrain roundnumber modified_humannext markerslist gamescores)

        )
        ((string-equal traininput "H")
            ;train to be updated is human 
            (cond 
                ;clearing the marker state to nil or empty so that if the train is marked it changes to unmarked.
                ((equal humannext nil)
                    (let* (
                        (updated_markers (sethumantrainmarker markerslist nil) )
                        )
                        ( playround humantiles computertiles boneyardtiles updatedtrain computertrain mexicantrain roundnumber modified_humannext updated_markers gamescores)
                    )
                
                )
                (t
                    ( playround humantiles computertiles boneyardtiles updatedtrain computertrain mexicantrain roundnumber modified_humannext markerslist gamescores)
                )
            )                
        )
        ((string-equal traininput "C")
            ;train to be updated is computer" 
            (cond 
                ;clearing the marker state to nil or empty so that if the train is marked it changes to unmarked.
                ((equal humannext t)
                    (let* (
                        (updated_markers (setcomputertrainmarker markerslist nil) )
                        ) 
                        ( playround humantiles computertiles boneyardtiles humantrain updatedtrain mexicantrain roundnumber  modified_humannext updated_markers gamescores)
                    )
                )
                (t
                    ( playround humantiles computertiles boneyardtiles humantrain updatedtrain mexicantrain roundnumber  modified_humannext markerslist gamescores)
                )
            )
        )   
        (t (print "Invalid train played"))  
    )    
)

;/* *********************************************************************
;Function Name: GetComputerhelp
;Purpose: To allow the human player to get help from the computer.
;           Helps to get the best tile and best  train to play as per computer judgement.
;Parameters:    1)humantiles -->list of tiles of human player
;               2)computertiles -->list of tiles of computer player
;               3)boneyardtiles --> list of tiles allocated to boneyard
;               4)humantrain --> list that contains tiles of human train 
;               5)computertrain --> list that contains tiles of computer train
;               6)mexicantrain --> list that contains tiles of mexicantrain
;               7)roundnumber -->current round number of the game
;               8)humannext -->variable that denotes if human player's turn is next stores T or nil
;               9)markerslist --> computer and human train marked status in list along with continous turns played 
;                       variable in format (marker marker continousturns) or (nil nil 0)
;               10)gamescores --> game scores of the players  
;Return Value: List that helps to display the best move to play 
;Algorithm:   
;               1)Check if forced to play orphan double train and get max tile that can be played on orphan double train
;               2)Check if mexican train is started or not. If not try to start it
;               3)Play the double tile that is possible on any valid train
;               4)Check if you can avoid playing on a train if you played double before
;               5)Display the maxtile to play and trian
;               6)Get tile from boneyard
;Assistance Received: none
;********************************************************************* */
(defun GetComputerhelp(humantiles computertiles boneyardtiles humantrain computertrain mexicantrain roundnumber humannext markerslist gamescores)

    (let* (
            (mustplayorphantrain (Orphandoubleexists humantrain computertrain mexicantrain markerslist))
            (Ishumanorphan  (Isorphantrain humantrain))
            (Iscomputerorphan  (Isorphantrain computertrain))
            (Ismexicanorphan  (Isorphantrain mexicantrain))
        )
        (cond
            ;this is the case orphan double exists so tile must be played here.
            ((equal mustplayorphantrain t)
                ;play the orphan train
                (cond
                    ((and Ishumanorphan (playabletileexists humantiles humantrain))
                        (let* (
                            (tiletoplay (getplayablemax humantiles  humantrain (list -1 -1))))
                            (Displayhelpmsg "Human" tiletoplay "H train is an orphan double train.")
                        )   
                    )
                    ((and Iscomputerorphan (playabletileexists humantiles computertrain))
                        (let* (
                            (tiletoplay (getplayablemax humantiles  computertrain (list -1 -1))))
                            (Displayhelpmsg "Computer" tiletoplay "C train is an orphan double train.")
                        )
                    )
                    ((and Ismexicanorphan (playabletileexists humantiles mexicantrain))
                        (let* (
                            (tiletoplay (getplayablemax humantiles  mexicantrain (list -1 -1))))
                            (Displayhelpmsg "Mexican" tiletoplay "M train is an orphan double train.")
                        )
                    )
                    (t
                        (print "Since Human player doesnot have any tiles to play to orphan double trian, Pick a tile from boneyard.")
                    )
                )
            )
            ;must start the mexican train if it has not yet been started.
            ((and (eq (length mexicantrain ) 1)(eq (playabletileexists humantiles mexicantrain) t))
                (let*
                    (
                        (tiletoplay (getplayablemax humantiles  mexicantrain (list -1 -1)))
                    )
                    (Displayhelpmsg "Mexican" tiletoplay " it helps to start the Mexican Train.")
                )
            )
            ;if there are double tile that can be played on valid trains.
            ((eq (DoubletileValidTrains humantiles humantrain computertrain mexicantrain (Computertrainmarked markerslist ))   t )
                (cond
                    ((eq (playabledoubleinTrain humantiles humantrain ) t)
                        (let*
                            ((tiletoplay (getplayabledoubletile humantiles humantrain)))
                            (Displayhelpmsg "Human" tiletoplay " it may help to create orphan double train")
                        )                                        
                    )
                    ((eq (playabledoubleinTrain humantiles mexicantrain ) t)
                        (let*
                            ((tiletoplay (getplayabledoubletile humantiles mexicantrain)))
                            (Displayhelpmsg "Mexican" tiletoplay "it may help to create orphan double train")
                        )   
                    )
                    ((and (eq (playabledoubleinTrain humantiles computertrain ) t) (eq (Computertrainmarked markerslist) t))
                        (let*
                            ((tiletoplay (getplayabledoubletile humantiles computertrain)))
                            (Displayhelpmsg "Computer" tiletoplay "computer train is marked and it may help to create orphan double train")
                        )   
                    )
                )       
            )
            ;if played double tile before and contain one more tile to play on a valid train.
            ((and (> (getcontinousturns markerslist) 0 ) (eq (playabletiletrains humantiles humantrain computertrain mexicantrain (Computertrainmarked markerslist)) t))
                (let* 
                    (   (maxtileHtrain   ( getplayablemax humantiles humantrain (list -1 -1)))
                        (maxtileCtrain   ( getplayablemax humantiles computertrain (list -1 -1)))
                        (maxtileMtrain   ( getplayablemax humantiles mexicantrain (list -1 -1)))
                        (Ctrainmarked  (Computertrainmarked markerslist) )   
                        (Htrainorphan  (Isorphantrain humantrain))
                        (Mtrainorphan  (Isorphantrain mexicantrain))
                        (Ctrainorphan  (Isorphantrain computertrain))
                        (validtileHtrain    (tilenotnull maxtileHtrain))
                        (validtileCtrain    (tilenotnull maxtileCtrain))
                        (validtileMtrain    (tilenotnull maxtileMtrain))
                    )
                    (cond 
                        ;If c train is orphan and can play in h train m train
                        ((and (or (eq validtileHtrain t) (eq validtileMtrain t) ) (eq Ctrainorphan t))
                            (cond
                                ((>= (sum maxtileHtrain) (sum maxtileMtrain)  )
                                    (Displayhelpmsg "Human" maxtileHtrain "helps for making orphan double train." )
                                )
                                (t
                                    (Displayhelpmsg "Mexican" maxtileMtrain "helps for making orphan double train." )
                                )
                            )  
                        )
                        ; if h train is orphan and can play in m train and c train
                        ((and (or (and (eq validtileCtrain t) (eq Ctrainmarked t ) ) (eq validtileMtrain t) ) (eq Htrainorphan t))
                            (cond
                                ((and (>= (sum maxtileCtrain) (sum maxtileMtrain))  (eq Ctrainmarked t)  )
                                    (Displayhelpmsg "Computer" maxtileCtrain "helps for making orphan double train." )
                                )
                                (t
                                    (Displayhelpmsg "Mexican" maxtileMtrain "helps for making orphan double train." )
                                ) 
                            ) 
                        )
                        ;if m train is orphan and can play in c trian and h train.
                        ((and (or (and (eq validtileCtrain t) (eq Ctrainmarked t) ) (eq validtileHtrain t) ) (eq Mtrainorphan t))
                            (cond
                                ((and (>= (sum maxtileCtrain) (sum maxtileHtrain))  (eq Ctrainmarked t) )
                                    (Displayhelpmsg "Computer" maxtileCtrain "helps for making orphan double train." )
                                )
                                (t
                                    (Displayhelpmsg "Human" maxtileHtrain "helps for making orphan double train." )
                                ) 
                            )  
                        )
                        (T  
                            ; if you can play nowhere except the double tile you placed earlier play max tile instead of picking from boneyard.
                            ( DisplayMaxtile humantiles computertiles boneyardtiles humantrain computertrain mexicantrain roundnumber humannext markerslist gamescores)
                        )
                    )
                )   
            )
            ;get maxtile to play on any train.
            ((eq (playabletiletrains humantiles humantrain computertrain mexicantrain (Computertrainmarked markerslist)) t)
                (DisplayMaxtile humantiles computertiles boneyardtiles humantrain computertrain mexicantrain roundnumber humannext markerslist gamescores)
            )
            ;got no tiles to play
            (t
                (print "Human doesnot have any valid tiles to play. Please pick one from boneyard and continue")
            ))))

;/* *********************************************************************
;Function Name: Computerplayerstrategy
;Purpose: To allow the computer to play itself.
;Parameters:    1)humantiles -->list of tiles of human player
;               2)computertiles -->list of tiles of computer player
;               3)boneyardtiles --> list of tiles allocated to boneyard
;               4)humantrain --> list that contains tiles of human train 
;               5)computertrain --> list that contains tiles of computer train
;               6)mexicantrain --> list that contains tiles of mexicantrain
;               7)roundnumber -->current round number of the game
;               8)humannext -->variable that denotes if human player's turn is next stores T or nil
;               9)markerslist --> computer and human train marked status in list along with continous turns played 
;                       variable in format (marker marker continousturns) or (nil nil 0)
;               10)gamescores --> game scores of the players  
;Return Value:  functions based on best strategy to play
;
;Algorithm:     1)Check if forced to play orphan double train and get max tile that can be played on orphan double train
;               2)Check if mexican train is started or not. If not try to start it
;               3)Play the double tile that is possible on any valid train
;               4)Check if you can avoid playing on a train if you played double before
;               5)Display the maxtile to play and trian
;               6)Get tile from boneyard
;Assistance Received: none
;********************************************************************* */
(defun Computerplayerstrategy(humantiles computertiles boneyardtiles humantrain computertrain mexicantrain roundnumber humannext markerslist gamescores)

    (let* (
            (mustplayorphantrain (Orphandoubleexists humantrain computertrain mexicantrain markerslist))
            (Ishumanorphan  (Isorphantrain humantrain))
            (Iscomputerorphan  (Isorphantrain computertrain))
            (Ismexicanorphan  (Isorphantrain mexicantrain))
        )
        (cond
            ;this is the case orphan double exists so tile must be played here.
            ((equal mustplayorphantrain t)
                ;play the orphan train
                (cond
                    ((and Ishumanorphan (playabletileexists computertiles humantrain))
                        ; orphan train will be played by tile from the computer tiles list to the human orphan train
                        (print "Since the human train was orphan double train.")
                        (ForwardtiletraintoPlay (getplayablemax computertiles  humantrain (list -1 -1)) "H" humantrain computertrain mexicantrain humantiles computertiles
                            boneyardtiles roundnumber humannext markerslist gamescores)
                        
                    )
                    ((and Iscomputerorphan (playabletileexists computertiles computertrain))
                        ;computer train is an orphan train and tiles can be played here.
                        (print "Since the computer train was orphan double train.")
                        (ForwardtiletraintoPlay (getplayablemax computertiles  computertrain (list -1 -1)) "C" humantrain computertrain mexicantrain humantiles computertiles
                            boneyardtiles roundnumber humannext markerslist gamescores)
                    )
                    ((and Ismexicanorphan (playabletileexists computertiles mexicantrain))
                        ;;computer train is an orphan train and tiles can be played here.  
                        (print "Since the mexican train was orphan double train.")
                        (ForwardtiletraintoPlay (getplayablemax computertiles  mexicantrain (list -1 -1)) "M" humantrain computertrain mexicantrain humantiles computertiles
                            boneyardtiles roundnumber humannext markerslist gamescores)
                    )
                    (t
                        ;;tile cannot be played to orphan double train
                        ;;need to pick atile from boneyard and see if it can be played on a orphan double train.
                        (print "Since computer didnot have any tiles to play to orphan double trian boneyard tile is picked!")
                        (Placeboneyardtoorphan humantiles computertiles boneyardtiles humantrain computertrain mexicantrain roundnumber humannext markerslist
                        gamescores)
                    )
                
                )
            )
            ;must start the mexican train if it has not yet been started.
            ((and (eq (length mexicantrain ) 1)(eq (playabletileexists computertiles mexicantrain) t))
                (print "Since mexican train was not started! Computer started the mexican train with largest tile.")
                (ForwardtiletraintoPlay (getplayablemax computertiles  mexicantrain (list -1 -1)) "M" humantrain computertrain mexicantrain humantiles computertiles
                            boneyardtiles roundnumber humannext markerslist gamescores)
                
            )
            ;must play a double train in case their is a valid double train to play
            ((eq (DoubletileValidTrains computertiles computertrain humantrain mexicantrain (Humantrainmarked markerslist ))   t )
                (print "Since the double tile is playable computer will try to make orphan double train.")
                (cond
                    ;can play double tile in human train
                    ((and (eq (playabledoubleinTrain computertiles humantrain ) t) (eq (Humantrainmarked markerslist) t)) 
                        (ForwardtiletraintoPlay (getplayabledoubletile computertiles humantrain) "H" humantrain computertrain mexicantrain humantiles computertiles
                            boneyardtiles roundnumber humannext markerslist gamescores)   

                    )
                    ;can play double tile in computer train
                    ((eq (playabledoubleinTrain computertiles computertrain ) t)  
                        (ForwardtiletraintoPlay (getplayabledoubletile computertiles computertrain) "C" humantrain computertrain mexicantrain humantiles computertiles
                            boneyardtiles roundnumber humannext markerslist gamescores)                                    
                    )
                    ;can play double tile in mexican train
                    ((eq (playabledoubleinTrain computertiles mexicantrain ) t) 
                        (ForwardtiletraintoPlay (getplayabledoubletile computertiles mexicantrain) "M" humantrain computertrain mexicantrain humantiles computertiles
                            boneyardtiles roundnumber humannext markerslist gamescores) 
                    )
                )
            )
            ;try to play on a train  that helps for creating orphan double train..
            ((and (> (getcontinousturns markerslist) 0 ) (eq (playabletiletrains computertiles computertrain humantrain mexicantrain (Humantrainmarked markerslist)) t))
                
                (let* 
                    (   (maxtileHtrain   ( getplayablemax computertiles humantrain (list -1 -1)))
                        (maxtileCtrain   ( getplayablemax computertiles computertrain (list -1 -1)))
                        (maxtileMtrain   ( getplayablemax computertiles mexicantrain (list -1 -1)))
                        (Htrainmarked  (Humantrainmarked markerslist) )
                        (Htrainorphan  (Isorphantrain humantrain))
                        (Mtrainorphan  (Isorphantrain mexicantrain))
                        (Ctrainorphan  (Isorphantrain computertrain))
                        (validtileHtrain    (tilenotnull maxtileHtrain))
                        (validtileCtrain    (tilenotnull maxtileCtrain))
                        (validtileMtrain    (tilenotnull maxtileMtrain))
                    )
                    (cond 
                        ((and (or (eq validtileCtrain t) (eq validtileMtrain t) ) (eq Htrainorphan t))
                            (cond
                                ((>= (sum maxtileCtrain) (sum maxtileMtrain)  )
                                    (print  "In order to create an orphan double train Computer avoided H train and played the maxtile to C train")
                                    (ForwardtiletraintoPlay maxtileCtrain "C" humantrain computertrain mexicantrain humantiles computertiles boneyardtiles roundnumber 
                                            humannext markerslist gamescores)   
                                )
                                (t
                                    (print "In order to create an orphan double train Computer avoided H train and played the maxtile to M train") 
                                    (ForwardtiletraintoPlay maxtileMtrain "M" humantrain computertrain mexicantrain humantiles computertiles boneyardtiles roundnumber 
                                            humannext markerslist gamescores)
                                )
                            ) 
                           
                        )
                        ((and (or (and (eq validtileHtrain t) (eq Htrainmarked t ) ) (eq validtileMtrain t) ) (eq Ctrainorphan t))
                            (cond
                                ((and (>= (sum maxtileHtrain) (sum maxtileMtrain))  (eq Htrainmarked t)  )
                                    (print "In order to create an orphan double train Computer avoided C train and played the maxtile on H train")
                                    (ForwardtiletraintoPlay maxtileHtrain "H" humantrain computertrain mexicantrain humantiles computertiles boneyardtiles roundnumber 
                                            humannext markerslist gamescores)
                                )
                                (t
                                    (print "In order to create an orphan double train Computer avoided C train and played the maxtile to M train")
                                    (ForwardtiletraintoPlay maxtileMtrain "M" humantrain computertrain mexicantrain humantiles computertiles boneyardtiles roundnumber 
                                            humannext markerslist gamescores) 
                                ) 
                            ) 
                         
                        )
                        ((and (or (and (eq validtileHtrain t) (eq Htrainmarked t) ) (eq validtileCtrain t) ) (eq Mtrainorphan t))
                            (cond
                                ((and (>= (sum maxtileHtrain) (sum maxtileCtrain))  (eq Htrainmarked t) )
                                    (print "In order to create an orphan double train Computer avoided M train and played the maxtile to H train")
                                    (ForwardtiletraintoPlay maxtileHtrain "H" humantrain computertrain mexicantrain humantiles computertiles boneyardtiles roundnumber 
                                            humannext markerslist gamescores)
                                )
                                (t
                                    (print "In order to create an orphan double train Computer avoided M train and played the maxtile to C train")
                                    (ForwardtiletraintoPlay maxtileCtrain "C" humantrain computertrain mexicantrain humantiles computertiles boneyardtiles roundnumber 
                                            humannext markerslist gamescores) 
                                ) 
                            ) 
                           
                        )
                        (T
                            (print "Since there were no tiles to play on other trains, Orphan train could not be created.")
                            ( GetMaxtileandPlay humantiles computertiles boneyardtiles humantrain computertrain mexicantrain roundnumber humannext markerslist gamescores)
                        )
                    )
                )
               
            )
            ((eq (playabletiletrains computertiles computertrain humantrain mexicantrain (Humantrainmarked markerslist)) t)
                ( GetMaxtileandPlay humantiles computertiles boneyardtiles humantrain computertrain mexicantrain roundnumber humannext markerslist gamescores)
            )
            (t
                (print "Since computer doesnot have anytiles it will pickup from boneyard.")
                (Automaticboneyardtotrain humantiles computertiles boneyardtiles humantrain computertrain mexicantrain roundnumber humannext markerslist gamescores)
            )
        )))


;/* *********************************************************************
;Function Name: playtiletotrain
;Purpose: This function helps to move a tile to a train while also modifying the train, playertiles and markers in the process
;Parameters:    1)humantiles -->list of tiles of human player
;               2)computertiles -->list of tiles of computer player
;               3)boneyardtiles --> list of tiles allocated to boneyard
;               4)humantrain --> list that contains tiles of human train 
;               5)computertrain --> list that contains tiles of computer train
;               6)mexicantrain --> list that contains tiles of mexicantrain
;               7)roundnumber -->current round number of the game
;               8)humannext -->variable that denotes if human player's turn is next stores T or nil
;               9)markerslist --> computer and human train marked status in list along with continous turns played 
;                       variable in format (marker marker continousturns) or (nil nil 0)
;               10)gamescores --> game scores of the players 
;               11) trainisplayable --> if the train is playable or not.
;               12) tileinput --> the index if the tile to be played
;               13) traintoplay --> trian to be played
;               14) tile --> tile in format (a b)
;               15) enginetile --> engine tile for the round
;               16) traininput --> value of train played in character
;Return Value:  
;Algorithm:   
;Assistance Received: none
;********************************************************************* */        
(defun playtiletotrain(trainisplayable tileinput traintoplay tile humantiles computertiles boneyardtiles
                humantrain computertrain mexicantrain roundnumber enginetile traininput humannext markerslist gamescores)

    (cond 
        ((eq trainisplayable t)
            ;these function help to remove the tiles from the usertiles and add to the chosen train
            (LET* 
                ;currentplayer tiles function gets the tile of the current user 
                ;removetile removes the given tile from the current user tiles
                (   (updatedplayertiles (removeTile tileinput (Currentplayertiles humannext humantiles computertiles)) )
                    (rotatedtile (rotatetile traintoplay tile) )
                    (updatedplayedtrain (addTiletoEnd traintoplay rotatedtile))
                    (doubleplayed (Checkdoubletile tile))
                    ;modify next player in case tile is double
                    (modified_humannext (RepeatPlayer doubleplayed humannext))
                    ;save the numbers of turns played by the same player before.
                    (up_markerslist (setcontinousturnplayed markerslist doubleplayed))
                    
                )
            ;-------------------------------expression below this---------------------------------------------
            (cond
                ;if computer train is not marked and human player tries to play computer train.
                ( (and (and (and (eq humannext nil) (eq (Computertrainmarked markerslist) nil))  (string-equal traininput "C")) (equal (Isorphantrain computertrain) nil)  )
                    (print "Sorry human player cannot play tile on umarked computer train")
                    (playround humantiles computertiles boneyardtiles humantrain computertrain mexicantrain roundnumber (reverseturn humannext) markerslist gamescores)
                )
                ;if human train is not marked and computer player tries to play human train.
                ( (and (and (and (eq humannext t) (eq (Humantrainmarked markerslist) nil))  (string-equal traininput "H"))  (equal (Isorphantrain humantrain) nil) )
                    (print "Sorry computer player cannot play tile on umarked human train")
                    (playround humantiles computertiles boneyardtiles humantrain computertrain mexicantrain roundnumber (reverseturn humannext) markerslist gamescores)
                )
                (t 
                    ;following lines help to display user if the tile was placed succesfully
                    (print tile)
                    (princ " tile was placed on the ")
                    (princ traininput)
                    (princ " train")
                    (displaydoublemsg tile)
                    ( updateplayertiles updatedplayertiles updatedplayedtrain humantiles computertiles boneyardtiles
                    humantrain computertrain mexicantrain roundnumber enginetile traininput humannext up_markerslist modified_humannext gamescores)
                ))))
        ;user needs to pick a boneyard tile or replay the move
        ;(reverseturn humannext) helps to make sure same player plays again instead of alternating the turns as move is not completed.
        (t (print "Tile you chose is not playable on the given train! Please play again")
           (playround humantiles computertiles boneyardtiles humantrain computertrain mexicantrain roundnumber (reverseturn humannext) markerslist gamescores) 
        )            
    )
)

;/* *********************************************************************
;Function Name: boneyardtoplayertiles
;Purpose:   this function helps to move the tiles from a boneyard to player tiles list if picked from boneyard not playable
;           If playable calls the playboneyardtile function to move to the one of the valid trains.
;
;Parameters:    1)humantiles -->list of tiles of human player
;               2)computertiles -->list of tiles of computer player
;               3)boneyardtiles --> list of tiles allocated to boneyard
;               4)humantrain --> list that contains tiles of human train 
;               5)computertrain --> list that contains tiles of computer train
;               6)mexicantrain --> list that contains tiles of mexicantrain
;               7)roundnumber -->current round number of the game
;               8)humannext -->variable that denotes if human player's turn is next stores T or nil
;               9)markerslist --> computer and human train marked status in list along with continous turns played 
;                       variable in format (marker marker continousturns) or (nil nil 0)
;               10)gamescores --> game scores of the players 
;               11)enginetile -->engine tile for the current round.
;Return Value:  functions based on the tiles and playable trains list.
;Algorithm:     1)If no tiles on boneyard mark the train and continue
;               2) If tile on boneyard and playable on one of the train callPlayboneyardtile function
;               3 If tile on boneyard and not playable add the tile to playerslist and continue.
;Assistance Received: none
;********************************************************************* */
(defun boneyardtoplayertiles(humantiles computertiles boneyardtiles  humantrain computertrain mexicantrain roundnumber enginetile humannext markerslist gamescores)
    (cond ((equal (length boneyardtiles) 0)
            (print "There were no tiles to pick from boneyard so player train is marked.")
            (cond
                ;current player is human
                ((equal humannext nil)
                    (let 
                        (
                            (updated_marker (sethumantrainmarker markerslist t) )
                        )
                        (playround humantiles computertiles boneyardtiles humantrain computertrain mexicantrain roundnumber humannext updated_marker gamescores)
                    )
                )             
            )
        )
        (t          
            (let* 
                (( tiletoadd (list (first boneyardtiles))) 
                    (currentplayer_tiles (currentplayertiles humannext humantiles computertiles))
                    (updatedtiles (addTiletoEnd currentplayer_tiles tiletoadd))
                    (updatedboneyard (rest boneyardtiles))    
                )
                ;--------------------output of the boneyard tile---------------
                (princ "Boneyard tile is:")
                (princ tiletoadd)
                ;----------------------------------------------------------------
                (cond
                    ;this should only run if the human player is taking tiles from the boneyard.
                    ((eq humannext nil) 
                        ;human tiles need to be updated in this case.
                        ;boneyard is played by human player
                        (cond
                            ;in case the boneyard tile is playable give user choice of train to play
                            ;else continue the round 
                            ((eq (boneyardtileplayable tiletoadd humantrain computertrain mexicantrain humannext markerslist) t)
                                ;if tile is playable human player gets to choose where to place the tile.
                                (Playboneyardtile humantiles computertiles boneyardtiles humantrain computertrain mexicantrain roundnumber enginetile humannext markerslist tiletoadd gamescores) 
                            )
                            (t 
                                (let*
                                    ;marked the human train to true as boneyard tile was not playable.
                                    ((added_marker (sethumantrainmarker markerslist t)))
                                    (print "Boneyard tile is not playable so human player turn is skipped and tile is added to list.Marker added.")
                                    (playround updatedtiles computertiles updatedboneyard humantrain computertrain mexicantrain roundnumber humannext added_marker gamescores)
                                )
                                
                            ))))))))


;/* *********************************************************************
;Function Name: Playboneyardtile
;Purpose: In order to play the boneyard tiles chosen by human player on one of the trains.
;Parameters:    1)humantiles -->list of tiles of human player
;               2)computertiles -->list of tiles of computer player
;               3)boneyardtiles --> list of tiles allocated to boneyard
;               4)humantrain --> list that contains tiles of human train 
;               5)computertrain --> list that contains tiles of computer train
;               6)mexicantrain --> list that contains tiles of mexicantrain
;               7)roundnumber -->current round number of the game
;               8)humannext -->variable that denotes if human player's turn is next stores T or nil
;               9)markerslist --> computer and human train marked status in list along with continous turns played 
;                       variable in format (marker marker continousturns) or (nil nil 0)
;               10)gamescores --> game scores of the players 
;               11)enginetile -->enginetile for the round
;               12)tiletoadd -->boneyard tile picked
;Return Value:  functions based on the user input and train available to play.
;Algorithm:     1) Check if there is an orphan double train that human player needs to play
;               2) If not ask for the train that player wants to play.
;               
;Assistance Received: none
;********************************************************************* */
(defun Playboneyardtile ( humantiles computertiles boneyardtiles humantrain computertrain mexicantrain roundnumber enginetile humannext markerslist
    tiletoadd gamescores)
    (print "Please enter the train where you want to place the playable boneyard tile>>")
    (let*
        (   (traininput (readtrain))
            (traintoplay (Gettrain traininput humantrain computertrain mexicantrain ))
            (tileplayable (checktrain traintoplay tiletoadd))
            (rotatedtile (rotatetile traintoplay tiletoadd))
            (updatedboneyard (rest boneyardtiles))
            (doubletile (Checkdoubletile tiletoadd))
            ;modified humannext helps to determine if the player is to be called again or not based on the double tile.
            (modified_humannext (RepeatPlayer doubletile humannext))
            ;this statement help to set the number of turn same players has played
            ;value stored inside markerslist (t t 0)--> 3rd param
            (markcontinousturn (setcontinousturnplayed markerslist doubletile))
            (orphanexists (Orphandoubleexists humantrain computertrain mexicantrain markerslist))
            (orphanplayed (Isorphantrain traintoplay))
        )
        (cond 
            ;orphan double train must be played if there is any.
            ( (and (equal orphanexists t) (equal orphanplayed nil))
                (print "You must play orphan double train!")
                ( Playboneyardtile humantiles computertiles boneyardtiles humantrain computertrain mexicantrain roundnumber enginetile humannext markerslist
                    tiletoadd gamescores )
            )   
            ;in this case tile is playable on the user chosen train. need to further check if train chosen is opponent train
            ((eq tileplayable t)
                (cond 
                    ;In this case current player is human and it must be as computer player doesnot choose manually.
                    ((eq humannext nil)
                        ;user wants to put the boneyard tile to human train
                        (cond 
                            ((string-equal traininput "H")
                                (let* 
                                    (
                                        (updatedhumantrain (addTiletoEnd humantrain rotatedtile))
                                        (updated_markers (Erasemarker "H" "H" markcontinousturn ))
                                    )
                                    ;--------------------expressions below this---------------------------------------
                                    (print "Boneyard tile has been played to the human train")              
                                    ( displaydoublemsg tiletoadd)

                                    (playround humantiles computertiles updatedboneyard updatedhumantrain computertrain mexicantrain roundnumber modified_humannext updated_markers gamescores)
                            ))
                            ;user wants to put the boneyard to to mexican train
                            ((string-equal traininput "M")
                                (let* (
                                        (updatedmexicantrain (addTiletoEnd mexicantrain rotatedtile ))
                                    )
                                    ;--------------------expressions below this---------------------------------------
                                    (print "Boneyard tile has been played to the mexican train")
                                    ( displaydoublemsg tiletoadd)
                                    (playround humantiles computertiles updatedboneyard humantrain computertrain updatedmexicantrain roundnumber  modified_humannext markcontinousturn gamescores)
                                ))
                            ;user wants to put the boneyard tile to the computer train.
                            ;in this case checks if the computer train is marked or not.
                            ((or (and (string-equal traininput "C") (eq (Computertrainmarked markerslist) t)) (Isorphantrain computertrain))
                                (let* (
                                        (updatedcomputertrain (addTiletoEnd computertrain rotatedtile ))
                                        (updated_markers (Erasemarker "H" "C" markcontinousturn ))
                                    )   
                                    ;--------------------expressions below this---------------------------------------
                                    (print "Boneyard tile has been played to the computer train")
                                    ( displaydoublemsg tiletoadd)
                                    (playround humantiles computertiles updatedboneyard humantrain updatedcomputertrain mexicantrain roundnumber  modified_humannext updated_markers gamescores)
                            ))
                            ;in this case, tile is not playable on input train. So user is asked again to enter train to play.
                            (t  (print "Invalid choice!!Pick valid train between Human, Mexican and Computer train.")
                                (Playboneyardtile humantiles computertiles boneyardtiles humantrain computertrain mexicantrain roundnumber enginetile humannext markerslist
                                tiletoadd gamescores))
                        ) 
                    )
                )
            )
            ;in this case train chosen is invalid so rechoose the train.
            (t 
                (print "Invalid choice!!Pick valid train between Human, Mexican and Computer train.")
                (Playboneyardtile humantiles computertiles boneyardtiles humantrain computertrain mexicantrain roundnumber enginetile humannext markerslist
                tiletoadd gamescores)
            )
        )  
    )
)


;/* *********************************************************************
;Function Name: boneyardtileplayable 
;Purpose: check if the tile picked from boneyard is playbale on atleast one of the trains.
;Parameters:    1)tile --> tile picked from boneyard
;               4)humantrain --> list that contains tiles of human train 
;               5)computertrain --> list that contains tiles of computer train
;               6)mexicantrain --> list that contains tiles of mexicantrain
;               8)humannext -->variable that denotes if human player's turn is next stores T or nil
;               9)markerslist --> computer and human train marked status in list along with continous turns played 
;                       variable in format (marker marker continousturns) or (nil nil 0)
;Return Value:  t if tile is playable nil otherwise
;Algorithm:     1) check if orphan double train exists: If exists check if there is a tile to play orphan double train.
;               2) If orphan double doesnot exist check if human and mexican train can be played with matching tile.
;               3) check if computer train has marker and if it does check if a matching tile can be played.
;Assistance Received: none
;********************************************************************* */
( defun boneyardtileplayable( tile humantrain computertrain mexicantrain humannext markerslist)
    (let*
        (   
            ;orphanexists stores t or nil based on if orphan double train is present.
            (orphanexists (Orphandoubleexists humantrain computertrain mexicantrain markerslist))
        )
        (cond (
            ;if there is orphan double present
            (equal orphanexists t)
                (cond
                    ;orphan is human train and human train is playable-returns true 
                    ( (and (equal (Isorphantrain humantrain) t )  (equal (checktrain humantrain tile ) t ) ) 
                        t
                    )
                    ;orphan is computer train and computer train is playable-returns true 
                    ( (and (eq (Isorphantrain computertrain) t )  (equal (checktrain computertrain tile ) t)  )
                        t
                    )
                    ;orphan is mexican train and mexican train is playable-returns true 
                    ( (and (eq (Isorphantrain mexicantrain) t )  (equal (checktrain mexicantrain tile ) t)  )
                        t
                    )
                    ;else orphan train is not playable
                    (t
                        nil
                    )
                )
            )
            ;orphan train does not exist.
            (t   
                (cond
                    ;current player is human
                    ((eq humannext nil)
                        (cond 
                            ;playable on human train
                            ((eq (checktrain humantrain tile) t) 
                                t   
                            )
                             ;playable on mexican train
                            ((eq (checktrain mexicantrain tile) t) 
                                t   
                            )
                             ;playable on computer train because of the marker 
                            (( and (eq (checktrain computertrain tile) t) (eq (Computertrainmarked markerslist) t) ) 
                                t   
                            )
                            ;not playable on anytrain
                            (t 
                                nil
                            )))
                    (t (print "Invalid input for next player!!!"))
                )))))


;/* *********************************************************************
;Function Name: CheckOrphanandPlay
;Purpose:   Check if there is orphan train or not and allow to play other trains only if there is no orphan train.  
;Parameters:    1)humantiles -->list of tiles of human player
;               2)computertiles -->list of tiles of computer player
;               3)boneyardtiles --> list of tiles allocated to boneyard
;               4)humantrain --> list that contains tiles of human train 
;               5)computertrain --> list that contains tiles of computer train
;               6)mexicantrain --> list that contains tiles of mexicantrain
;               7)roundnumber -->current round number of the game
;               8)humannext -->variable that denotes if human player's turn is next stores T or nil
;               9)markerslist --> computer and human train marked status in list along with continous turns played 
;                       variable in format (marker marker continousturns) or (nil nil 0)
;               10)gamescores --> game scores of the players
;               11)trainisplayable --> if the tile can be played to train
;               12) tileinput --> index of tile to be played
;               13)traintoplay --> tiles of the train to be played
;               14)tile --> tile to play in format (a b)
;               15)enginetile --> engine tile for current round
;               16)traininput --> character value for played train
;               17)orphanexists -->if there is an orphan double train
;               18)orphanplayed --> if orphan train is played
;Return Value:  
;Algorithm:     1)if orphan train exists and orphan not played call playround to make a move again.
;               2) if no orphan double train, can play any train.  
;Assistance Received: none
;********************************************************************* */
(defun CheckOrphanandPlay(trainisplayable tileinput traintoplay tile humantiles computertiles boneyardtiles
                humantrain computertrain mexicantrain roundnumber enginetile traininput humannext markerslist orphanexists orphanplayed gamescores)
        (cond 
            ;in this orphan train exists but the tile isnot played on the orphan double train.
            ((and (equal orphanexists t) (equal orphanplayed nil))
                (print "Sorry you must play the orphan double train!")
                (playround humantiles computertiles boneyardtiles humantrain computertrain mexicantrain roundnumber (reverseturn humannext) markerslist gamescores)
            )
            ;orphan double train doesnot exist.
            (t 
                (playtiletotrain trainisplayable tileinput traintoplay tile humantiles computertiles boneyardtiles
                            humantrain computertrain mexicantrain roundnumber enginetile traininput humannext markerslist gamescores)
            )
        )
)

;/* *********************************************************************
;Function Name: Automaticboneyardtotrain
;Purpose: to move the boneyard tile picked by computer player to one of the trains or the computer's pile of tiles
;Parameters:    1)humantiles -->list of tiles of human player
;               2)computertiles -->list of tiles of computer player
;               3)boneyardtiles --> list of tiles allocated to boneyard
;               4)humantrain --> list that contains tiles of human train 
;               5)computertrain --> list that contains tiles of computer train
;               6)mexicantrain --> list that contains tiles of mexicantrain
;               7)roundnumber -->current round number of the game
;               8)humannext -->variable that denotes if human player's turn is next stores T or nil
;               9)markerslist --> computer and human train marked status in list along with continous turns played 
;                       variable in format (marker marker continousturns) or (nil nil 0)
;               10)gamescores --> game scores of the players
;Return Value: playround function with parameters based on tile and trains available. 
;Algorithm:    Since this function is only called:
;               1)if there is no orphan double train place the tile on one of the valid trains.
;               2)Place the tile to the computer's list of tiles.
;Assistance Received: none
;********************************************************************* */
(defun Automaticboneyardtotrain( humantiles computertiles boneyardtiles humantrain computertrain mexicantrain roundnumber humannext markerslist gamescores)
    (Print "Boneyard Tile: ")
    (Princ (first boneyardtiles))
    (cond
        ;human train has marker andt tile is playable on human train.
        ((and (eq (checktrain humantrain (list (first boneyardtiles)) ) t)  (eq (Humantrainmarked markerslist) t))
            ;needs to play boneyard tile to human train
            (Print "Computer picked the boneyard tile and placed it to the human train")
            (playround  humantiles computertiles (rest boneyardtiles) (addTiletoEnd humantrain (list (first boneyardtiles)) )  
                computertrain mexicantrain roundnumber humannext markerslist gamescores  )
        )
        ;tile is playable on mexican train
        ((eq (checktrain mexicantrain (list (first boneyardtiles)) ) t)
            ;needs to play boneyard tile to mexican train
            (Print "Computer picked the boneyard tile and placed it to the mexican train")
            (playround  humantiles computertiles (rest boneyardtiles) humantrain 
                computertrain (addTiletoEnd mexicantrain (list(first boneyardtiles)) )  roundnumber humannext markerslist gamescores  )
        )
        ;tile is playable on computer traim
        ((eq (checktrain computertrain (list (first boneyardtiles)) ) t)
            ;needs to play boneyard tile to computer train
            (Print "Computer picked the boneyard tile and placed it to the computer train")
            (playround  humantiles computertiles (rest boneyardtiles) humantrain 
                (addTiletoEnd computertrain (list(first boneyardtiles)) ) mexicantrain  roundnumber humannext markerslist gamescores  )
        )
        ;tilecannot be played anywhere.
        (T
            (print "Since the boneyard tile is not playable to any of the trains! Tile placed to computer's list of tiles.")
            (let* 
                (
                    (updated_markers (setcomputertrainmarker markerslist t))
                ) 
                (playround  humantiles (addTiletoEnd computertiles (list (first boneyardtiles))) (rest boneyardtiles) humantrain 
                computertrain mexicantrain  roundnumber humannext updated_markers gamescores  )
            ))))

;/* *********************************************************************
;Function Name: ForwardtraintoPlay
;Purpose:   gettilenumber of the tiles and tiles of the trian to play before calling playtiletotrain function.
;Parameters:    1)humantiles -->list of tiles of human player
;               2)computertiles -->list of tiles of computer player
;               3)boneyardtiles --> list of tiles allocated to boneyard
;               4)humantrain --> list that contains tiles of human train 
;               5)computertrain --> list that contains tiles of computer train
;               6)mexicantrain --> list that contains tiles of mexicantrain
;               7)roundnumber -->current round number of the game
;               8)humannext -->variable that denotes if human player's turn is next stores T or nil
;               9)markerslist --> computer and human train marked status in list along with continous turns played 
;                       variable in format (marker marker continousturns) or (nil nil 0)
;               10)gamescores --> game scores of the players 
;               11)tiletoplay --> tile of the format (a b)
;               12)trainletter --> character that represents the one of the train.
;
;
;Return Value: playtiletotrain function that modifies the tiles of players and the trains. 
;Algorithm:     get the tilenumber and list of tiles of traintoplay based on input parameters and call playtiletotrain function. 
;Assistance Received: none
;********************************************************************* */
(defun ForwardtiletraintoPlay(tiletoplay trainletter humantrain computertrain mexicantrain humantiles computertiles boneyardtiles roundnumber 
            humannext markerslist gamescores)
    (let*
        (
            (tilenumber ( getTilelocation (length computertiles) computertiles tiletoplay))
            (traintoplay (Gettrain trainletter humantrain computertrain mexicantrain ))
        )
        (playtiletotrain t (+ tilenumber 1) traintoplay (list tiletoplay) humantiles computertiles boneyardtiles
                    humantrain computertrain mexicantrain roundnumber (GetengineTile roundnumber) trainletter humannext markerslist gamescores) 
    ))

;/* *********************************************************************
;Function Name: GetMaxTileandPlay
;Purpose: This function deals with getting the largest tile possible and playing for computer player.
;Parameters:    1)humantiles -->list of tiles of human player
;               2)computertiles -->list of tiles of computer player
;               3)boneyardtiles --> list of tiles allocated to boneyard
;               4)humantrain --> list that contains tiles of human train 
;               5)computertrain --> list that contains tiles of computer train
;               6)mexicantrain --> list that contains tiles of mexicantrain
;               7)roundnumber -->current round number of the game
;               8)humannext -->variable that denotes if human player's turn is next stores T or nil
;               9)markerslist --> computer and human train marked status in list along with continous turns played 
;                       variable in format (marker marker continousturns) or (nil nil 0)
;               10)gamescores --> game scores of the players
;Return Value:  playtiletotrain function with parameters based on the tile and train available.
;Algorithm:     find the train where maxtile can be played and play that train. 
;Assistance Received: none
;********************************************************************* */    
(defun GetMaxtileandPlay(humantiles computertiles boneyardtiles humantrain computertrain mexicantrain roundnumber humannext markerslist gamescores)

    (let* 
        (
            (Htrainplayable  (playabletileexists computertiles humantrain))
            (Ctrainplayable  (playabletileexists computertiles computertrain))
            (Mtrainplayable  (playabletileexists computertiles mexicantrain))
            (maxtileHtrain   ( getplayablemax computertiles humantrain (list -1 -1)))
            (maxtileCtrain   ( getplayablemax computertiles computertrain (list -1 -1)))
            (maxtileMtrain   ( getplayablemax computertiles mexicantrain (list -1 -1)))
            (Htrainmarked  (Humantrainmarked markerslist) )
            (maxtrain (first (getmaxTiletrain  maxtileHtrain maxtileCtrain maxtileMtrain Htrainmarked)))
            (tiletoplay (second (getmaxTiletrain  maxtileHtrain maxtileCtrain maxtileMtrain Htrainmarked)))
            (tilenumber ( getTilelocation (length computertiles) computertiles tiletoplay))
            (traintoplay (Gettrain maxtrain humantrain computertrain mexicantrain ))
        )
        ;--------------------------prints the largest tile possible to play and train where it can be played-----------------------------
        (print "The largest tile possible was:")  
        (princ tiletoplay)
        (Print "So playing the tile on:")
        (princ maxtrain)
        (princ " train")
        ;----Plays  the largest tile on the train possible---------------------------------------------------------------------------------
        (cond 
            ((string-equal maxtrain "H")
                (playtiletotrain t (+ tilenumber 1) traintoplay (list tiletoplay) humantiles computertiles boneyardtiles
                                        humantrain computertrain mexicantrain roundnumber (GetengineTile roundnumber) "H" humannext markerslist gamescores)
            )
            ((string-equal maxtrain "C")
                (playtiletotrain t (+ tilenumber 1) traintoplay (list tiletoplay) humantiles computertiles boneyardtiles
                                        humantrain computertrain mexicantrain roundnumber (GetengineTile roundnumber) "C" humannext markerslist gamescores)
            )
            ((string-equal maxtrain "M")
                (playtiletotrain t (+ tilenumber 1) traintoplay (list tiletoplay) humantiles computertiles boneyardtiles
                                        humantrain computertrain mexicantrain roundnumber (GetengineTile roundnumber) "M" humannext markerslist gamescores)
            )
        )
    )
)

;/* *********************************************************************
;Function Name:getMaxTiletrain 
;Purpose:  This function gives back the train where maxtile can be played and the tile that is max.
;           It is used for the computer player to get a train where max tile can be played
;Parameters:    1)maxtileopponenttrain --> maxtile that can be played on opponent train
;               2)maxtileselftrain --> maxtile that can be played on self train
;               3)maxtileMtrain -->maxtile that can be played on mexican train
;               4)opponenttrainmarked --> t or nil value based on the marker on opponents train. 
;Return Value:  List of character for the max train along with the max tile
;Algorithm:     1)Check if maxtile can be played on opponent train and opponent is marked.
;               2)Check if maxtile can be played on self train.
;               3)Check if maxtile can be played on mexican train.
;               4)if  opponenttrain not marked find the max train between mexican and self train
;Assistance Received: none
;********************************************************************* */

(defun getmaxTiletrain(maxtileopponenttrain maxtileselftrain maxtileMtrain opponenttrainmarked)  

    (cond 
        ;h train is marked and has max tile
        ((and (and (eq opponenttrainmarked t) (>= (sum maxtileopponenttrain) (sum maxtileselftrain))) (>= (sum maxtileopponenttrain) (sum maxtileMtrain))) 
            (cons 'H (cons maxtileopponenttrain ()))
        )
        ;computer or self train has maxtile
        ((and ( >= (sum maxtileselftrain) (sum maxtileopponenttrain))  (>= (sum maxtileselftrain) (sum maxtileMtrain)))
            (cons 'C (cons maxtileselftrain ()))
        )
        ;mexican train can match the max tile
        ((and ( >= (sum maxtileMtrain) (sum maxtileopponenttrain))  (>= (sum maxtileMtrain) (sum maxtileselftrain))) 
            (cons 'M (cons maxtileMtrain ()))
        )
        ;maxtile can be played on mexican again
        (( >= (sum maxtileMtrain) (sum maxtileselftrain))   
            (cons 'M (cons maxtileMtrain ()))
        )
        ;c train for the maxtile.
        (t   
            (cons 'C (cons maxtileselftrain ()))
        )
    )
)

;/* *********************************************************************
;Function Name: getmaxTiletrainhelp
;Purpose: This function gives back the train where maxtile can be played and the tile that is max.
;           This function is used for human help to find a train where human player can play max tile.
;Parameters:    1)maxtileopponenttrain --> maxtile that can be played on opponent train
;               2)maxtileselftrain --> maxtile that can be played on self train
;               3)maxtileMtrain -->maxtile that can be played on mexican train
;               4)opponenttrainmarked --> t or nil value based on the marker on opponents train. 
;Return Value:  List of character for the max train along with the max tile
;Algorithm:     1)Check if maxtile can be played on opponent train and opponent is marked.
;               2)Check if maxtile can be played on self train.
;               3)Check if maxtile can be played on mexican train.
;               4)if  opponenttrain not marked find the max train between mexican and self train
;Assistance Received: none
;********************************************************************* */
;
(defun getmaxTiletrainhelp(maxtileopponenttrain maxtileselftrain maxtileMtrain opponenttrainmarked)  

    (cond 
        ((and (and (eq opponenttrainmarked t) (>= (sum maxtileopponenttrain) (sum maxtileselftrain))) (>= (sum maxtileopponenttrain) (sum maxtileMtrain))) 
            (cons 'C (cons maxtileopponenttrain ()))
        )
        ((and ( >= (sum maxtileselftrain) (sum maxtileopponenttrain))  (>= (sum maxtileselftrain) (sum maxtileMtrain)))
            (cons 'H (cons maxtileselftrain ()))
        )
        ((and ( >= (sum maxtileMtrain) (sum maxtileopponenttrain))  (>= (sum maxtileMtrain) (sum maxtileselftrain))) 
            (cons 'M (cons maxtileMtrain ()))
        )
        (( >= (sum maxtileMtrain) (sum maxtileselftrain))   
            (cons 'M (cons maxtileMtrain ()))
        )
        (t   
            (cons 'H (cons maxtileselftrain ()))
        )
    )
)

;/* *********************************************************************
;Function Name: playabletiletrains
;Purpose: This function helps to detect if there exists a playable tile on self train opponent train or mexican train.
;Parameters:    1)selftiles --> tile of the player
;               2)selftrain --> tiles of the train of th player
;               3)opponenttrain --> tiles of opponent train
;               4)mexicantrain --> tiles of mexican train 
;               5)opponentmarked --> t or nil value based on if opponent train is marked or not.
;Return Value:  t if there is a playable tile nil otherwise.
;Algorithm:     1)check if there is a matching tile to play on self or mexican train.
;               2)check if oppoent train has marker and check if tiles can be played.
;               
;Assistance Received: none
;********************************************************************* */

(defun playabletiletrains(selftiles selftrain opponenttrain mexicantrain opponentmarked)
     (cond
        ((eq (playabletileexists selftiles selftrain ) t)
            t
        )
        ((eq (playabletileexists selftiles mexicantrain ) t)
            t
        )
        ((and (eq (playabletileexists selftiles opponenttrain ) t) (eq opponentmarked t)    )
            t
        )
        (T
            nil
        )))

;/* *********************************************************************
;Function Name: DoubletileValidTrains
;Purpose: This function checks if there is a double tile in players list of tiles 
;               that can be played in any of the valid trains.
;Parameters:    1)playertiles --> tile of the player
;               2)selftrain --> tiles of the train of th player
;               3)opponenttrain --> tiles of opponent train
;               4)mexicantrain --> tiles of mexican train 
;               5)markeronopponent --> t or nil value based on marker on opponent train.
;Return Value:  t if there is a double tile nil otherwise.
;Algorithm:   Check if there is a playable double on self , mexican or opponent train. 
;               in case of opponent train also need to check marker.
;Assistance Received: none
;********************************************************************* */

(defun DoubletileValidTrains( playertiles selftrain opponenttrain mexicantrain markeronopponent) 
    (cond
        ((eq (playabledoubleinTrain playertiles selftrain ) t)
            t
        )
        ((eq (playabledoubleinTrain playertiles mexicantrain ) t)
            t
        )
        ((and (eq (playabledoubleinTrain playertiles opponenttrain ) t) (eq markeronopponent t)    )
            t
        )
        (T
            nil
        )
    
    )
)

;/* *********************************************************************
;Function Name: playabledoubleinTrain
;Purpose: This function gives t or nil value based on whether there is a double tile that can be played on the given train.
;Parameters:    1)tiles --> list of tiles of the of a player
;               2) train --> tiles of a train.
;Return Value:  t or nil value based on whether there is a double tile that can be played on a train.
;Algorithm:   1)Check if a first tile of the tileslist is double and matches the last tile of train.
;               -if yes return t
;             2) call recursively with rest of the tiles
;             3) if no tiles left return nil.
;Assistance Received: none
;********************************************************************* */

(defun playabledoubleinTrain(tiles train)
   (let*
        (   
            ;first tile of tiles list
            (tile_sidea (first (first tiles)) )
            (tile_sideb (second (first tiles)))  
            (trainend  (first (reverse train))) 
            (tiletomatch (second trainend))
        )
        (cond
            ((eq (length tiles) 0) 
                nil
            )
            ((and (eq tile_sidea tiletomatch) ( eq tile_sideb tile_sidea)) 
                t
            )
            (t 
                (playabledoubleinTrain (rest tiles) train)    
            )))) 

;/* *********************************************************************
;Function Name: getplayabledoubletile
;Purpose: This gives a playable double tile in case it exists in the given train
;           This function is only called if there is a playable double in train.
;Parameters:     1)tiles --> list of tiles of the of a player
;                2) train --> tiles of a train.
;Return Value:  tile if there is a double tile, nil otherwise.
;Algorithm:     1)Check if a first tile of the tileslist is double and matches the last tile of train.
;                   -if yes return tile
;               2) call recursively with rest of the tiles
;               3) if no tiles left return nil.
;Assistance Received: none
;********************************************************************* */

(defun getplayabledoubletile(tiles train)
   (let*
        (   
            ;first tile of tiles list
            (tile_sidea (first (first tiles)) )
            (tile_sideb (second (first tiles)))  
            (trainend  (first (reverse train))) 
            (tiletomatch (second trainend))
        )
        (cond
            ((eq (length tiles) 0) 
                nil
            )
            ((and (eq tile_sidea tiletomatch) ( eq tile_sideb tile_sidea)) 
                (cons tile_sidea (cons tile_sideb ()))
            )
            (t  
                (getplayabledoubletile (rest tiles) train)    
            )))) 

;/* *********************************************************************
;Function Name: Placeboneyardtoorphan
;Purpose: Boneyard tile is placed on the orphan double train if possible else added to the players list.
;           This function is called when there is an orphan double train and computer player picks tile from the boneyard.
;Parameters:    1)humantiles -->list of tiles of human player
;               2)computertiles -->list of tiles of computer player
;               3)boneyardtiles --> list of tiles allocated to boneyard
;               4)humantrain --> list that contains tiles of human train 
;               5)computertrain --> list that contains tiles of computer train
;               6)mexicantrain --> list that contains tiles of mexicantrain
;               7)roundnumber -->current round number of the game
;               8)humannext -->variable that denotes if human player's turn is next stores T or nil
;               9)markerslist --> computer and human train marked status in list along with continous turns played 
;                       variable in format (marker marker continousturns) or (nil nil 0)
;              10)gamescores --> game scores of the players
;Return Value: playround functions with parameters based on train and tiles available. 
;Algorithm:   1) if tile is playable on any of the orphan double train , place the tile.   
;             2) else place the tile to computer players list.  
;
;Assistance Received: none
;********************************************************************* */

( defun Placeboneyardtoorphan ( humantiles computertiles boneyardtiles humantrain computertrain mexicantrain roundnumber humannext markerslist gamescores)
    
    (let* 
        (
            (tiletoadd (list (car boneyardtiles)))
            (updatedboneyard  (rest boneyardtiles))
            (updatedhumantrain (addTiletoEnd humantrain tiletoadd) )
            (updatedcomptrain (addTiletoEnd computertrain tiletoadd))
            (updatedmexicantrain  (addTiletoEnd mexicantrain tiletoadd))
            (updatedmarker (setcomputertrainmarker markerslist nil ))
            (updatedtileslist (addTiletoEnd computertiles tiletoadd))
        )
        (print "Boneyard tile: ")
        (princ tiletoadd)
        (cond
            ((and (eq (checktrain humantrain tiletoadd) t) (eq (Isorphantrain humantrain) t))
                (print "Boneyard is placed on the human train.")
                (playround humantiles computertiles updatedboneyard updatedhumantrain computertrain mexicantrain roundnumber humannext markerslist gamescores) 
            )
            ((and (eq (checktrain computertrain tiletoadd) t) (eq (Isorphantrain computertrain) t))
                (print "Boneyard tile is placed on the computer train.")
                (playround humantiles computertiles updatedboneyard humantrain updatedcomptrain mexicantrain roundnumber humannext updatedmarker gamescores)
            )
            ((and (eq (checktrain mexicantrain tiletoadd) t) (eq (Isorphantrain mexicantrain) t))
                (print "Boneyard tile is placed on the mexican train.")
                (playround humantiles computertiles updatedboneyard humantrain computertrain updatedmexicantrain roundnumber humannext markerslist gamescores)
            )
            (t 
                (print "Boneyard tile is placed to the computer's list of tiles as tile is not playable on Orphan double train.CTrain marked." )
                (let*
                    (
                        (updated_markers (setcomputertrainmarker markerslist t))
                    )
                    (playround humantiles updatedtileslist updatedboneyard humantrain computertrain mexicantrain roundnumber humannext updated_markers gamescores)
                )))))

;this function checks if a train can be played using any tile from the list of tiles.
;/* *********************************************************************
;Function Name: playabletileexists
;Purpose: check if the playbale tile exists on given train.
;Parameters:    tiles of the player and train to play.
;Return Value:  t or nil value based on if tile exists.
;Algorithm:   1)check if first tile match with train; if match return t
;             2)if not call function recursively with rest of tiles.
;             3)if no tiles left return nil.  
;Assistance Received: none
;********************************************************************* */
(defun playabletileexists(tiles train)
   (let*
        (   
            ;first tile of tiles list
            (tile_sidea (first (first tiles)) )
            (tile_sideb (second (first tiles)))  
            (trainend  (first (reverse train))) 
            (tiletomatch (second trainend))
        )
        (cond
            ((eq (length tiles) 0) 
                nil
            )
            ;tiles match so there is a tile that can be played.
            ((or (eq tile_sidea tiletomatch) ( eq tile_sideb tiletomatch)) 
                t
            )
            (t 
                (playabletileexists (rest tiles) train)    
            )))) 


;/* *********************************************************************
;Function Name: getplayablemax
;Purpose: this function gets the largest tile that can be played on the given train.
;Parameters:    1)tiles -->players tiles
;               2)train --> tiles of the train to play
;               3)finaltile -->maxtile as of now.
;Return Value:  maxtile
;Algorithm:     1) compare the first tile with current maxtile and store the maxtile as finaltile
;               2) call the function again recursively with rest of tile
;               3)compare rest of the tiles in the same way and return the tile when lenght is 0.
;Assistance Received: none
;********************************************************************* */
(defun getplayablemax (tiles train finaltile)
    (let*
            (   
                ;first tile of tiles list
                (tile_sidea (first (first tiles)) )
                (tile_sideb (second (first tiles))) 
                (trainend   (first (reverse train))  )  
                (tiletomatch (second trainend))
            )
            (cond
                ((eq (length tiles) 0) 
                    finaltile
                )
                ;in this case compares with finaltile than only updates the final tile
                ((or (eq tile_sidea tiletomatch) ( eq tile_sideb tiletomatch)) 
                    (getplayablemax (rest tiles) train (maxsum (first tiles) finaltile))
                )
                (t  
                    (getplayablemax (rest tiles) train finaltile)    
                )
            )
    )
)

;------------------helper functions for strategy of the game------------------------
;/* *********************************************************************
;Function Name: tilenotnull
;Purpose: check This function checks if a tile is a valid tile or not.
;               In case a tile is not valid it will have (-1 -1) as value.
;Parameters:    tile
;Return Value:  t or nil based on validity of tile.
;Algorithm:   if tile has (-1 -1) value tile is invalid else valid as other negative values are not assigned.
;Assistance Received: none
;********************************************************************* */

(defun tilenotnull(tile) 
    (cond
        ((and (eq (first tile ) -1) (eq (second tile ) -1))
            nil
        )
        (t 
            t
        )
    )
)

;/* *********************************************************************
;Function Name: sum
;Purpose: to get sum of two sides of a tile
;Parameters:    tile
;Return Value: sum of two sides of tile 
;Algorithm:   add two sides of tile
;Assistance Received: none
;********************************************************************* */
(defun sum(tile)

   (+ (first tile) (second tile))

)

;this function compares two tiles and returns a tile whose sum is greater for two sides.
;/* *********************************************************************
;Function Name: maxsum
;Purpose: compare the two tiles and return a tile with a max sum of two sides.
;Parameters:    tilea and tileb
;Return Value:  tile whichever has largest sum of sides
;Algorithm:   1) if sum of tilea>sum of tileb : return sidea 
;             2) return tileb otherwise 
;Assistance Received: none
;********************************************************************* */
(defun maxsum(tilea tileb)

    (cond( (>  (+ (first tilea ) (second tilea))  (+ (first tileb ) (second tileb)))
            tilea
        )
        (T
            tileb
        )
    
    )
)


;/* *********************************************************************
;Function Name: trainstarted
;Purpose: This function checks if the train has been played before or not based on length of tiles.
;Parameters: train--> tiles of the train
;Return Value:  t or nil based on whether train has been started or not.
;Algorithm:   --
;Assistance Received: none
;********************************************************************* */
(defun trainstarted(train)
    (cond ((= (length train) 1 )
            t    
        )
        (t 
            nil
        )
    )
)


;/* *********************************************************************
;Function Name: addcomputermarker
;Purpose:   This function adds marker at the start of the train which is used before writing to a file.  
;Parameters:    1)tiles--> tiles of the computertrain
;               2)markerslist --> list that contains the state of train markers
;Return Value:  list with possible marker at the start of the list
;Algorithm:     if the state of marker is equal to t, add marker to the start of the list
;Assistance Received: none
;********************************************************************* */
(defun addcomputermarker(tiles markerslist)

    (cond 
        ((equal (Computertrainmarked markerslist) t)
            (cons 'M tiles)
        )
        (T
            tiles
        )
    
    )
)


;/* *********************************************************************
;Function Name: nextplayername
;Purpose:   This function returns the name of the next player: human or computer
;Parameters:    humannext --> t or nil value on next player to play
;Return Value:  HUMAN or COMPUTER based on humannext value
;Algorithm:   1) if humannext is t --return HUMAN
;             2) return COMPUTER otherwise.
;Assistance Received: none
;********************************************************************* */
(defun nextplayername(humannext)
    (cond((eq humannext t)
            'HUMAN
        ) 
        (T
            'COMPUTER
        )  
    )   

)

;/* *********************************************************************
;Function Name: addhumanmarker
;Purpose:   This function adds marker at the end of the train which is used before writing to a file.  
;Parameters:    1)tiles--> tiles of the computertrain
;               2)markerslist --> list that contains the state of train markers
;Return Value:  list with possible marker at the start of the list
;Algorithm:     if the state of marker is equal to t, add marker to the start of the list
;Assistance Received: none
;********************************************************************* */
(defun addhumanmarker(tiles markerslist)

    (cond 
        ((equal (Humantrainmarked markerslist) t)
            (addTiletoEnd tiles (cons 'M ()) )
        )
        (T
            tiles
        )))


;/* *********************************************************************
;Function Name: Gettrain
;Purpose: This function returns back the list of tiles of a train where the input is a char that represents the train.
;Parameters:    1) traininput -->character value related to the train
;               2)humantrain --> tiles for human train
;               3)computertrain --> tiles for computer train
;               4)mexicantrain--> tiles for mexican train
;Return Value:  tiles of a train based on traininput value
;Algorithm:   check the value of the traininput and return list of train tiles based on that.
;Assistance Received: none
;********************************************************************* */ 
(defun Gettrain(traininput humantrain computertrain mexicantrain )
    (cond ((string-equal traininput "H") humantrain )
            ((string-equal traininput "C") computertrain)
            ((string-equal traininput "M") mexicantrain)
            (t (print "Invalid train input entered!")
                nil
            )   
    )               
)


;/* *********************************************************************
;Function Name: checktrain
;Purpose: This function checks if the last tile of the train is a match with the tile the user wants to add with
;Parameters:    1)train --> train to be played on
;               2)tile --> tile to be played
;Return Value:  t or nil value based on if train is playable or not.
;Algorithm:   1)IF either side of tile matches with side 2 of train return t, nil otherwise.
;Assistance Received: none
;********************************************************************* */
(defun checktrain(train tile) 
    ;since the sublist gives list inside list getting the first element using the car.
    (let
        (
            (trainlasttile (car (sublist train (- (length train) 1) 1 )))
            (tiletoplay (car tile))
        )
        (cond 
            ;this checks side 1
            ((eq (car (cdr trainlasttile)) (car tiletoplay) )
                t
            )
            ;this checks side 2
            ((eq (car (cdr trainlasttile)) (car (cdr tiletoplay)) )
                t
            )
            (t nil)
        )
    )    
)


;/* *********************************************************************
;Function Name: rotateTile 
;Purpose:  This function rotates the sides of the train tiles which enable printing tiles in matching order. 
;Parameters:    1)train--> train where the tile is to be added
;               2)tile--> tile to be added.
;Return Value:  tile 
;Algorithm:     1) check if the side of the train are matching against each other
;               2)if not move side1 to side2 and side2 to side1.
;Assistance Received: none
;********************************************************************* */
(defun rotateTile(train tile)
    (let*
        (
            (trainlasttile (car (sublist train (- (length train) 1) 1 )))
            (tiletoplay (car tile))
        )
        (cond 
            ;this checks side 1
            ((eq (car (cdr trainlasttile)) (car tiletoplay) )
               (list tiletoplay)
            )
            ;this checks side 2
            ((eq (car (cdr trainlasttile)) (car (cdr tiletoplay)) )
               (list (list (second tiletoplay) (first tiletoplay)))
            )
            (t nil)
        )))


;/* *********************************************************************
;Function Name: removeTile
;Purpose: Removes the tile of a given number from the tiles list 
;Parameters:    1)tile_number -->the index of tile to be removed.
;               2)tiles_list --> tiles list from where tile is to be removed.
;Return Value:  tiles with a tile removed.
;Algorithm:   1)if tilenumber is less than one return tiles list
;             2) if tilenumber is equal to 1 return rest of the tiles.
;             3)other wise call function recursively by decreasing tilenumber and removing one tile from front
;               so that when tilenumber is 1, it is the tile to remove.
;Assistance Received: none
;********************************************************************* */
(defun removeTile(tile_number tiles_list)
  (cond 
    ((< tile_number 1)
      tiles_list)
    ((= tile_number 1)
      (cdr tiles_list))
    (t
      (cons (car tiles_list) (removeTile (- tile_number 1) (cdr tiles_list))))))



;/* *********************************************************************
;Function Name: addTiletoEnd
;Purpose:   Adds the tiles to the end of the train
;           Here tile format is ((a,b )) which is same as tiles_list format.
;Parameters:    1)tiles_list --> where tile is to be added
;Return Value:  2)tile --> tile to be added
;Algorithm:   append tile at the end
;Assistance Received: none
;********************************************************************* */
(defun addTiletoEnd(tiles_list tile)
    (append tiles_list tile)
)


;/* *********************************************************************
;Function Name: RepeatPlayer
;Purpose: This function helps to allow to play again in case of the double tile by modifying human next value.
;Parameters:    1)doubletileplayed --> t or nil based on double tile is played or not
;               2)humannext --> t or nil based on next player
;Return Value:  t or nil  based on if a tile is double.
;Algorithm:     if double tile is played return the opposite value. 
;Assistance Received: none
;********************************************************************* */
(defun RepeatPlayer( doubletileplayed humannext )
    (cond
        ((equal doubletileplayed t)
            (cond
                ((equal humannext t)
                    nil
                )
                (t 
                    t
                )))
        (t 
            humannext
        )))


;/* *********************************************************************
;Function Name: reverseturn
;Purpose: This function changes the next players turn regardless of the tile played
;Parameters:    humannext
;Return Value:  opposite of humannext
;Algorithm:   flip value
;Assistance Received: none
;********************************************************************* */
(defun reverseturn(humannext)
    (cond
        ((eq humannext t)
            nil
        )
        (t 
           t 
        )))


;/* *********************************************************************
;Function Name: Currentplayertiles
;Purpose: returns the tile of the current player basesd on humannext value which determines who the current player is.
;Parameters:    1)humannext -->  t or nil based on next player
;               2)humatiles-->human player tiles
;               3)computertiles-->computer players tiles
;Return Value:  tiles of one player based on humans next
;Algorithm: 1) if next player is human return computers tiles
;           2)else return humans players tiles
;Assistance Received: none
;********************************************************************* */
(defun Currentplayertiles(humannext humantiles computertiles)
    (cond
        ((eq humannext t)
            computertiles
        )
        (t humantiles) 
    )
)

;/* *********************************************************************
;Function Name: sethumantrainmarker
;Purpose: This function sets the value to the markerslist
;           (nil nil 0)--> this is the format of the markerslist
;Parameters: 1)markerlist -->list that contains markers state
;            2) value -->value to be set
;Return Value:  markerslist with update value
;Algorithm:   set the first value of markerlist to value.
;Assistance Received: none
;********************************************************************* */
( defun sethumantrainmarker (markerlist value)
    (cons value (rest markerlist))
)


;/* *********************************************************************
;Function Name: setcomputertrainmarker
;Purpose: Sets computer marker to given value | T or nil
;Parameters:  1)markerlist -->list that contains markers state
;             2) value -->value to be set
;Return Value:  markerslist with update value
;Algorithm:     set the second value of markerslist to value  
;Assistance Received: none
;********************************************************************* */
( defun setcomputertrainmarker (markerlist value)
    (cons (first markerlist) (cons value (list (second (rest markerlist)))))
)


;/* *********************************************************************
;Function Name: getcontinousturns
;Purpose: This sets the continous number of turns played by the current player if the turn was repeated
;Parameters:markerslist
;Return Value:  number of turns played by the same player before.
;Algorithm:   --
;Assistance Received: none
;********************************************************************* */
(defun getcontinousturns(markerslist)
    (second (rest markerslist))
)


;/* *********************************************************************
;Function Name: Humantrainmarked
;Purpose: Checks if the humantrain is marked
;Parameters:    markerslist -->list that contains the state of the markers
;Return Value:  t or nil based on state of human train marker
;Algorithm:   --
;Assistance Received: none
;********************************************************************* */
(defun Humantrainmarked(markerlist)
    (cond
        ((eq (first markerlist) T)
            t
        )
        (t nil)
    ))


;/* *********************************************************************
;Function Name: Computertrainmarked
;Purpose: Returns if computer train is marked
;Parameters:    markerslist -->list that contains the state of the markers
;Return Value:  t or nil based on state of computer train marker
;Algorithm:  -- 
;Assistance Received: none
;********************************************************************* */
(defun Computertrainmarked(markerlist)
    (cond
    ((eq (second markerlist) T)
        t
    )
    (t nil)))


;/* *********************************************************************
;Function Name: returnscores
;Purpose: Returns the list containing human scores and the computerscore along with printing it.
;Parameters:    1) humanscore--> score of the human player for the round
;               2)computerscore-->score of the computer player for the round
;Return Value:  playerscores->list with humanscore and computerscore
;Algorithm:   
;Assistance Received: none
;********************************************************************* */
(defun returnscores(humanscore computerscore)
      (let* 
            (
                (playerscores  (cons humanscore(cons computerscore (list ()) )))   
            )
            (Printscoresforround humanscore computerscore)
            playerscores))


;/* *********************************************************************
;Function Name: setcontinousturnplayed
;Purpose: Sets continous turn played increases the number if double tile is played if non double tile is played it clears the
;               value to zero
;Parameters:    1)markerslist -->list that contains the state of the markers
;               2)doubleplayed --> t or nil based on tile
;Return Value:  updatedmarkerslist with number of turns played continously.
;Algorithm:   if a double tile is played increase the setcontinous value. set to 0 otherwise.
;Assistance Received: none
;********************************************************************* */
(defun setcontinousturnplayed (markerslist doubleplayed)

    (cond 
        ;if the double tile was played then increase the value of turns played by 1
        ((equal doubleplayed t)

            (let (
                (value (second (rest markerslist) ))
                )
                (cons (first markerslist) (cons (second markerslist) (cons (+ 1 value) ())))
            )
        )
        ;if non double tile was played set value to 0 as opponent player will play next
        (t
            (cons (first markerslist) (cons (second markerslist) (cons 0 ())))
        )))


;/* *********************************************
;Source Code for functions that read or write to file
;********************************************* */



;/* *********************************************************************
;Function Name: loadsavedfile
;Purpose: This function loads the content of the file into a list
;Parameters:    none
;Return Value:  list containing file contents
;Algorithm:   store the contents of the file provided by user and return the contents.
;Assistance Received: https://www.tutorialspoint.com/lisp/lisp_file_io.htm
;********************************************************************* */
(defun loadsavedfile()
    (print "Please enter the name of the file with .txt included: ")
    (let* (
            (IN (open (READ) :if-does-not-exist NIL))
            (savedgame ( cond 
                            (IN (read IN))
                            (t
                                (print "Wrong file name entered.")
                                (loadsavedfile)
                            )
                        )
            )
        )
        (cond
            ((null IN)
                nil)
            (t
                (close IN))
            )
        (print "File loaded for the new game!")
        savedgame
    ))


;/* *********************************************************************
;Function Name: SerializeandQuit
;Purpose: This function serializes the current state of the game and saves into a user given file name.
;Parameters:    1)humantiles -->list of tiles of human player
;               2)computertiles -->list of tiles of computer player
;               3)boneyardtiles --> list of tiles allocated to boneyard
;               4)humantrain --> list that contains tiles of human train 
;               5)computertrain --> list that contains tiles of computer train
;               6)mexicantrain --> list that contains tiles of mexicantrain
;               7)roundnumber -->current round number of the game
;               8)humannext -->variable that denotes if human player's turn is next stores T or nil
;               9)markerslist --> computer and human train marked status in list along with continous turns played 
;                       variable in format (marker marker continousturns) or (nil nil 0)
;              10)gamescores --> game scores of the players
;Return Value: --  
;Algorithm:  store the states of the game into multiple lists and merge 
;           them all  to into one list and store to the filename given by user.
;Assistance Received: https://gigamonkeys.com/book/files-and-file-io.html
;********************************************************************* */
(defun SerializeandQuit(humantiles computertiles boneyardtiles humantrain computertrain mexicantrain roundnumber humannext markerslist gamescores)

    (let* 
        (   
            (humanscore (first gamescores))
            (computerscore (second gamescores))
            (reversecomputer (reverse computertrain))
            (computerinnerreverse (reversecomputertrain reversecomputer (list )))
            (finalcompputertrain (addcomputermarker computerinnerreverse markerslist))
            (finalhumantrain (addhumanmarker humantrain markerslist))
            (nextplayer (nextplayername humannext))
            (finallist (cons roundnumber (cons computerscore (cons computertiles  (cons finalcompputertrain (cons humanscore(cons humantiles(cons finalhumantrain 
                (cons mexicantrain (cons boneyardtiles(cons nextplayer  ()  )))))))))))
        )
      
        (print "Please enter the filename(with.txt) you want to save file as")
        (LET* ((userInput (READ)))
             (with-open-file (stream userInput :direction :output)
                (format stream "~A~%" finallist)
            )
        
        )        
    )           
)




;/* *********************************************************************
;Function Name: Erasemarker
;Purpose: This function is used to remove a marker whenver a tile is played by the player on their train.
;Parameters:    1)player -->player who played the tiles
;               2)train -->train where tile is played
;               3)markerslit--> list of markers state
;Return Value:  updated markers
;Algorithm:   if the player plays on their own train remove the marker
;Assistance Received: none
;********************************************************************* */
(defun Erasemarker(player train markerslist)

    (cond 
        ((and (string-equal player "H") (string-equal train "H") )
            (sethumantrainmarker markerslist nil)
        )
          ((and (string-equal player "C") (string-equal train "C") )
            (setcomputertrainmarker markerslist nil)
        )
        (t 
            markerslist
        )
    )

)


;/* *********************************************************************
;Function Name: GetengineTile
;Purpose: This function returns an engine tile based on the current round number. 
;Parameters:    roundnumber
;Return Value:  enginetile in form ((a b)) 
;Algorithm:   subtract  roundnumber from 10 and and assign both sides of tile that number.
;Assistance Received: none
;********************************************************************* */
(defun GetengineTile(roundnumber) 
    (let*(
            (modround (mod roundnumber 10))
            (value (- 10 modround))
        )
        (list (cons value (cons value ())))
    ) 
)


;/* *********************************************************************
;Function Name: getTilelocation
;Purpose: This function finds the index of a tile in the given list, return nil if not found.
;Parameters:    1) size --> size of list
;               2)list --> list where tile is to be found
;               3)tile --> tile to be found
;Return Value:  index of the tile where it is found
;Algorithm:   decrease value of size everytime a tile doesnot match until the tile matches where size is the index of the tile.

;Assistance Received: none
;********************************************************************* */
(defun getTilelocation(size list tile) 
    (cond
        ((equal (nth size list ) tile )
            size
        )
        ((= size 0)
            nil
        )
        (t 
            (getTilelocation (- size 1) list tile)
        )
    )

)



;/* *********************************************************************
;Function Name: Checkdoubletile
;Purpose: This function returns if a given tile is a double tile.
;Parameters:    tile-->tile to be checked
;Return Value:  t or nil based on if tile is double
;Algorithm:   --
;Assistance Received: none
;********************************************************************* */
(defun Checkdoubletile(tile)
    (cond 
        ((equal  (first (first tile)) (second (first tile)))
            
            T
        )
        (t 
            nil 
        )
    )

)


;/* *********************************************************************
;Function Name: Orphandoubleexists
;Purpose: This function checks if there is a possible orphan double train among all the trains.
;Parameters:    1)humantrain --> tiles of humantrain
;               2)computertrain--> tiles of computer train
;               3)mexicantrain--> tiles of mexican train
;               4)markerslist--> state of players train marker
;Return Value:  t if there is any double tile at the end of any train, nil otherwise.
;Algorithm:   check the last tile of all the trains and if double return t, nil otherwise.
;Assistance Received: none
;********************************************************************* */
(defun Orphandoubleexists (humantrain computertrain mexicantrain markerslist)

    (cond 
        ;this runs if the player is playing a turn after a double tile in which orphan double isnot considered.
        ( (> (getcontinousturns markerslist ) 0 )
           nil
        )
        (t
            (let*
                (
                    (humanend (elt humantrain (- (length humantrain) 1 )))
                    (computerend (elt computertrain (- (length computertrain) 1 )))
                    (mexicanend (elt mexicantrain (- (length mexicantrain) 1 )))
                ) 
                (cond 
                    (( and (equal (Checkdoubletile (list humanend)) t)  (/= (length humantrain ) 1 ) )
                        t
                    )
                    ((and  (equal (Checkdoubletile (list computerend)) t)  (/= (length computertrain ) 1)       )
                        t
                    )
                    (( and (equal (Checkdoubletile (list mexicanend)) t) (/= (length mexicantrain ) 1)  )
                        t
                    )
                    (T
                        nil
                    ))))))


;/* *********************************************************************
;Function Name: Isorphantrain
;Purpose: This returns if a train given is an orphan train or not.
;Parameters:   train--> tiles of the train
;Return Value:  t or nil based on last tile of the train.
;Algorithm:   check if the last tile of the train is double or not.
;Assistance Received: none
;********************************************************************* */
(defun Isorphantrain(train) 
    (let*
        (
            (lasttile (elt train (- (length train) 1 ))))
            (cond
                ((/= (length train) 1)
                    (Checkdoubletile (list lasttile))
                )
                (t 
                    nil
                ))))


;/* *********************************************************************
;Function Name: totalscoreoftrain
;Purpose: This gives the total score when the remaining tiles of the player is given as input.
;Parameters: 1)vect --> tiles of the train
;            2)totalscore--> initial score without counting the current tiles
;Return Value:  totalscoreoftrain.
;Algorithm:   add the sum of first tile to total and recursively calculate sum of the rest of the tiles.
;Assistance Received: none
;********************************************************************* */
(defun totalscoreoftrain(vect total) 

    (cond  
        ((equal (length vect) 0)
            total
        )
        (T
            (let* 
                (   
                    (sum  (+ (first (car vect) ) (second (car vect) ) ))
                    (newtotal (+ total sum))
                )
                (totalscoreoftrain (rest vect) newtotal)
            ))))


;/* *********************************************************************
;Function Name: removecomputermarker
;Purpose: This function helps to erase computer marker from computer train tiles that contains a marker.
;           This function is used when the computer train tiles are extracted from user text file.
;Parameters: vect--> tiles of computer train along with possible marker at the front
;Return Value:  computertraintiles without marker
;Algorithm:     check if the first item of the list is list:
;                   -if it is list, no marker is present so no need to remove 
;                   -if it is not a list, there is a marker present so return rest of the tiles as computer train tiles.
;Assistance Received: none
;********************************************************************* */
(defun removecomputermarker(vect) 
    (cond 
        ((listp (car vect) )
          ;marker doesnot exists
            vect
        )
        (       
          ;marker  exists
            (rest vect)    
        )
    
    )   
)


;/* *********************************************************************
;Function Name:     removehumanmarker
;Purpose:           This function helps to erase human train marker from the train tiles taken as input from the file
;                   This function is used when the human train tiles are extracted from user text file.
;Parameters:        vect--> list of tile that may contain marker at the end
;Return Value:      computer train tiles without a marker
;Algorithm:         check if the last item of the list is list:
;                   -if it is list, no marker is present so no need to remove 
;                   -if it is not a list, there is a marker present so return rest of the tiles as human train tiles.
;Assistance Received: none
;********************************************************************* */
(defun removehumanmarker(vect) 
    (let*
        (   
            ;this is the first element of the list
            (possiblehumanmarker (elt vect (- (length vect) 1)))
        )
         
    (cond 
        ;if this is list it isnot marker so no need to remove ie.tile like (2,3) exists
        ((listp possiblehumanmarker )
            vect
        )
        (       
            ;marker  exists in form (M (2,3) (3,4))
            (removeTile (length vect) vect) 
        ))))


;/* *********************************************************************
;Function Name: humantrainmarker
;Purpose: This function checks if there is a marker on list of human train tiles and returns t or nil
;Parameters:    vect--> list of tile that may contain marker at the end
;Return Value:  t or nil based on the presence of marker
;Algorithm:   check if the last item of the list is list:
;                   -if it is list, no marker is present so return nil
;                   -if it is not a list, there is a marker present so return t.
;Assistance Received: none
;********************************************************************* */
(defun humantrainmarker(vect) 
    (let*
        (
            (possiblehumanmarker (elt vect (- (length vect) 1)))
        )   
    (cond 
        ((listp possiblehumanmarker )
            ;marker doesnot exists
            nil
        )
        (       
            ;marker  exists
            t
        ))))



;/* *********************************************************************
;Function Name: computertrainmarker
;Purpose: This function returns if the vect or computertraintiles has a marker at the begining of the list
;Parameters:    vect--> list of tile that may contain marker at the front
;Return Value:  t or nil based on the presence of marker
;Algorithm:     check if the last item of the list is list:
;                   -if it is list, no marker is present so return nil
;                   -if it is not a list, there is a marker present so return t.
;Assistance Received: none
;********************************************************************* */
(defun computertrainmarker(vect)
    (cond 
        ((listp (car vect) )
          ;marker doesnot exists
            nil
        )
        (       
          ;marker  exists
            t
        )
    
    )  
)


;/* *********************************************************************
;Function Name: reversecomputertrain
;Purpose: This function helps to reverse the computer tiles inside the individual tiles.
;Parameters:    1)vect -->tiles of computertrain
;               2)reversed -->part of tiles that are already reversed
;Return Value:  reversed computer train.
;Algorithm:     1)get the first tile and add to the end of reversedlist
;               2) call the function with rest of the tiles and do the same process with rest of the tiles until no tiles are left.
;Assistance Received: none
;********************************************************************* */
(defun reversecomputertrain(vect reversed)

    (cond 
        ((equal (length vect) 0)
            reversed
        )
        (t 
            (let*
                (
                    (firsttile (first vect) )
                    (rotatedtile (cons (second firsttile) (cons (first firsttile)  ()) ) )
                    (newreversed (addTiletoEnd  reversed (list rotatedtile) )  )
                )
                (reversecomputertrain (rest vect) newreversed)

            )        
        )
    )

)
 

;/* *********************************************************************
;Function Name: gethumannext
;Purpose: This function is used to read the next player from the input text file.
;Parameters:    nextplayer--> string value that contains the name of next player 
;Return Value:  t if the value is human , nil if computer 
;Algorithm:     ---- 
;Assistance Received: none
;********************************************************************* */
(defun gethumannext (nextplayer)

    (cond
        ((string-equal nextplayer "HUMAN")
            t
        )
        (T
            nil
        )
    )
)
;;------------------------end of functions-----------------------------------------------------------
(mexican-train)



