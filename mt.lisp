;;;starts the game by taking unput on whether to start a game or load it
(defun mexican-train()
    ( print "Please select one of the options")
    ( print "(1) to start a new game")
    ( print "(2) to load a saved game from a file")
    (LET ((useroption ( readnum 1 2)))  
        (print useroption) 
        (cond ((= useroption 1)
            ;starting a new game if the user enter 1
            (startgame useroption (list 0 0) 1))
            ;reading a game from file if the user presses 2
            (t (startgame useroption (list 0 0) 1))
        )
    )
)   
;;this function helps to read the filename from the user and sends for imput  validation
(defun readnum (start end)
    (print "Enter value in the range:")
    (princ start)
    (princ "-")
    (princ end)
    (TERPRI)
    (LET ((userInput (READ)))
    (validate userInput start end ))  )

;;this function checks if the number num is range start to end is valid or not
;;if it is not valid calls the number recursively else returns the  value
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
(defun readtrain ()
    (LET* ((userInput (READ)))
    (validtrain userInput ))  )

;;this function checks if the usertrain value is valid which is one of
;; computer user mexican or boneyard 
;;if not function gets called recursively
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


;;this function helps to get a valid train input from the user
(defun readcomputeroptions ()
    (LET* 
        (
            (userInput (READ))
        )
    (validcomputerinput userInput )) )

;;this function checks if the usertrain value is valid which is one of
;; computer user mexican or boneyard 
;;if not function gets called recursively
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

(defun nextstep() 
    (print "Press Y to play new game and N to quit")
    (LET* ((userInput (READ)))
        (validnextstep userInput )
    )
   
)

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

;;;this function checks the useroption and tries to start a new game
;;;by creating a new tiles deck or loading from the deck
(defun startgame (useroption gamescores round)
    (cond 
       ( (= useroption 1)
            (print "User decided to start a new game") 
            (print "----------------------------------------------------------------------------------")
            ;note I can also pass the deck to shuffle deck directly from here instead of using let
            ; and that is what lisp programming is all about.
            ;generating a deck of 55 tiles and shuffling it
            ;need to do a toss here in case of already played game

            (let* ( (deck (shuffledeck (generatedeck 9 9 )))
                    (enginetile (GetengineTile round))
                    (enginetile_pos (getTilelocation 54 deck (car enginetile)))
                    (finaldeck ( removeEnginetile (+ enginetile_pos 1) deck))
                    (humantiles (sublist finaldeck 0 16)) 
                    (computertiles (sublist finaldeck 16 16))
                    (boneyardtiles (sublist finaldeck 32 nil))
                    (playerscores  (startround humantiles computertiles boneyardtiles round t enginetile gamescores))
                    (humantotalscore (+ (first playerscores) (first gamescores)))
                    (computertotalscore (+ (second playerscores) (second gamescores)))
                    (scoreslist (cons humantotalscore (cons computertotalscore (list ()))))
                )
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
                            (
                                (nextstep_input (nextstep))
                            )
                            (cond
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
                                
                                )
                            
                            )
                        )
                    )
                )

                
                
                
                
            )
            
            
       )
       ( (= useroption 2)
            ;need to read a file here and start a game from here
            (print "User decided to load a saved game") 
            (let*
            (
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
                (playerscores (playround humanhand computerhand boneyardtiles filteredhumantrain reversecomputertrain mexicantrain round humannext markerslist gamescores))
                (humantotalscore (+ (first playerscores) (first gamescores)))
                (computertotalscore (+ (second playerscores) (second gamescores)))
                (scoreslist (cons humantotalscore (cons computertotalscore (list ()))))


            )

                (cond ((and (= (first playerscores) 0) (= (second playerscores) 0 ) )
                        (print "The game has been serialized thank you!")
                        t
                    )
                    (T
                        (print "The round has ended!!") 
                        (print "Game score of humanplayer is: ")
                        (princ  (+ (first playerscores) (first gamescores)))
                        (print "Game score of computerplayer is: ") 
                        (princ (+ (second playerscores) (second gamescores)))
                        (let* 
                            (
                                (nextstep_input (nextstep))
                            )
                            (cond
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
                                
                                )
                            
                            )
                        )

                    )
                )
                
                
                
             
            )
        )
       (t (print  "Invalid input please restart the game"))
    )   
)

;;;this needs to have  atomic input list output as we need a new list
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
           
        )
    )
)
;;function shuffle deck helps to shuffle the deck we have created to randomly organize the tiles in a deck
(DEFUN shuffledeck(deck)
    (cond
        ( (null deck)
        ())
        (t
            (LET*(
            ;
                (randomize (make-random-state t))
                (tile_position (random (length deck) randomize)))
                (cons
                ;ELT function accesses specified elements of sequences. The index is counted from zero. 
                    (elt deck tile_position)
                    (shuffledeck (moveTile(+ tile_position 1) deck))
                )
            )
        )
    )
)

;this function helps to move a tile at position tile_position to a randomized deck
(DEFUN moveTile(tile_position deck)
  (cond 
    ((< tile_position 1)
      deck)
    ((= tile_position 1)
      (cdr deck))
    (t
      (CONS (FIRST deck) (moveTile (- tile_position 1) (cdr deck))))
    )
)

;this function returns a sublist from index i for len elements
; nil will be return all the elements after index
;help taken from http://www.lee-mac.com/sublist.html
(defun sublist ( vector idx len )
    (cond
        (   (null vector) nil)
        (   (< 0  idx) (sublist (cdr vector) (- idx 1) len))
        (   (null len) vector)
        (   (< 0  len) (cons (car vector) (sublist (cdr vector) idx (- len 1))))
    )
)

;start round helps to initialize a round whereas it should call play round function to iteratively play a round until a round is over
;once a round is over play round will give access back to start round which will initialize a round again if needed.
(defun startround (humantiles computertiles boneyardtiles roundnumber humannext enginetile gamescores)
    
    ;Here all the trains and engine have tile 9-9 saved as this is a starting round
    ;(print (gamescore gamescores))
    (playround humantiles computertiles boneyardtiles enginetile enginetile enginetile  roundnumber  (setnextplayer gamescores) (list nil nil 0) gamescores)
   
    ;changes for new round to be made here.
)

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
                
                )
               
            )
        )
    
    )
)

(defun Printscoresforround(humanscore computerscore)

    (print "Human score for the round:  " )
    (princ humanscore)
    (print "Computerscore for the round  : ")
    (princ computerscore)
)

;this function should be used recursively to play rounds
(defun playround(humantiles computertiles boneyardtiles humantrain computertrain mexicantrain roundnumber humannext markerslist gamescores) 
    
    
    ( display humantiles computertiles boneyardtiles humantrain computertrain mexicantrain roundnumber (GetengineTile roundnumber) humannext markerslist gamescores)
    (print "--------------------------------------------------------------------------------------------------------------")
    (cond 
        ((equal (length humantiles) 0)
            (Print "User played all the tiles")
            (let* 
                (
                    (humanscore 0 )
                    (computerscore (totalscoreoftrain computertiles 0) ) 
                    (playerscores  (cons humanscore(cons computerscore (list ()) )))   
                )
                (Printscoresforround humanscore computerscore)
                playerscores
                
            )
           
        )
        ((equal (length computertiles) 0)
            (Print "computer played all the tiles")
            (let* 
                (
                    (humanscore (totalscoreoftrain humantiles 0) )
                    (computerscore 0 )  
                    (playerscores  (cons humanscore(cons computerscore (list ()) )))  
                )
                (Printscoresforround humanscore computerscore)
                playerscores  
            
            )
        )
        (  (and (and  (equal (Computertrainmarked markerslist ) t) (equal (Humantrainmarked markerslist ) t))   (equal (length boneyardtiles) 0)   )
            (print "boneyard is empty and both players train is marked so game is over")
            (let* 
                (
                    (humanscore (totalscoreoftrain humantiles 0) )
                    (computerscore (totalscoreoftrain computertiles 0) )  
                    (playerscores  (cons humanscore(cons computerscore (list ()) )))  
                )
                (Printscoresforround humanscore computerscore)
                playerscores
            )
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
                                (print "human serialized and quit")
                                (list 0 0)
                            )
                            ((string-equal traininput "A")
                                (GetComputerhelp humantiles computertiles boneyardtiles humantrain computertrain mexicantrain roundnumber humannext markerslist gamescores)
                                (playround humantiles computertiles boneyardtiles humantrain computertrain mexicantrain roundnumber humannext markerslist gamescores)
                            )
                            (t 
                                (let* (
                                    (tileinput (readnum 1 (length humantiles)))
                                    ;tile is the actual tile(x y) that user chose to play to the desired train
                                    (tile (sublist humantiles (- tileinput 1) 1 ))
                                    ;train to play has the tiles of the train that user wants to place the tile to 
                                    (traintoplay (Gettrain traininput humantrain computertrain mexicantrain ))
                                    ;trainisplayable holds if the user train is playable for the tileinput
                                    (trainisplayable (checktrain traintoplay tile))
                                    (orphanexists (Orphandoubleexists humantrain computertrain mexicantrain markerslist))
                                    (orphanplayed (Isorphantrain traintoplay)))
                                    ;;--------------variables above---------------------------
                                    ;once the tile is played to the train it should call itself recursively so that game continues
                                    (CheckOrphanandPlay trainisplayable tileinput traintoplay tile humantiles computertiles boneyardtiles
                                    humantrain computertrain mexicantrain roundnumber (GetengineTile roundnumber) traininput nil markerslist orphanexists orphanplayed gamescores)
                                    ;this should be the last thing to run before this function exits.
                                    ;-------------------------end of expressions----------------------------------------------------

                                )
                            )
                        )
        
                    )
                    
                )
                (t
                    (Displaynextplayer "Computer")
                    (LET ( ;character input for the train that the computer wants to place the tile to.
                            ;this should read options to play or serialize rather than getting other options.
                            (traininput (readcomputeroptions))  
                        )
                        ;----------------------------------expression below use the variable above-----------------
                        (cond 
                            ((string-equal traininput "S")
                                ;user should serialize and quit
                                (SerializeandQuit humantiles computertiles boneyardtiles humantrain computertrain mexicantrain roundnumber humannext markerslist gamescores)
                                (print "computer serialized and quit")
                                ;returning an empty list incase game is serialized.
                                (list 0 0)
                            )
                            (t 
                                (Computerplayerstrategy humantiles computertiles boneyardtiles humantrain computertrain mexicantrain roundnumber t markerslist gamescores)
                            )
                        )
        
                    )
                    
                )
            )
        )



    )

)

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
                                (tiletoplay (getplayablemax humantiles  humantrain (list -1 -1)))   
                            )
                            (Displayhelpmsg "Human" tiletoplay "H train is an orphan double train.")
                        )
                        
                    )
                    ((and Iscomputerorphan (playabletileexists humantiles computertrain))
                        (let* (
                                (tiletoplay (getplayablemax humantiles  computertrain (list -1 -1))) 
                            )
                            (Displayhelpmsg "Computer" tiletoplay "C train is an orphan double train.")
                        )
                    )
                    ((and Ismexicanorphan (playabletileexists humantiles mexicantrain))
                        (let* (
                                (tiletoplay (getplayablemax humantiles  mexicantrain (list -1 -1))) 
                            )
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
            ((eq (DoubletileValidTrains humantiles humantrain computertrain mexicantrain (Computertrainmarked markerslist ))   t )
                (cond
                    ((eq (playabledoubleinTrain humantiles humantrain ) t)
                        (let*
                            (
                                (tiletoplay (getplayabledoubletile humantiles humantrain))  
                            )
                            (Displayhelpmsg "Human" tiletoplay "it may help to create orphan double train")
                        
                        )                                        
                    )
                    ((eq (playabledoubleinTrain humantiles mexicantrain ) t)
                        (let*
                            (
                                (tiletoplay (getplayabledoubletile humantiles mexicantrain))
                            )
                            (Displayhelpmsg "Mexican" tiletoplay "it may help to create orphan double train")
                        )   
                    )
                    ((and (eq (playabledoubleinTrain humantiles computertrain ) t) (eq (Computertrainmarked markerslist) t))
                        (let*
                            (
                                (tiletoplay (getplayabledoubletile humantiles computertrain))
                            )
                            (Displayhelpmsg "Computer" tiletoplay "computer train is marked and it may help to create orphan double train")
                        )   
                    )
                    (T
                        (print "error occured")
                    ) 
                )       
            )
            ((and (> (getcontinousturns markerslist) 0 ) (eq (playabletiletrains humantiles humantrain computertrain mexicantrain (Computertrainmarked markerslist)) t))
                
                (let* 
                    (
                        (Htrainplayable  (playabletileexists humantiles humantrain))
                        (Ctrainplayable  (playabletileexists humantiles computertrain))
                        (Mtrainplayable  (playabletileexists humantiles mexicantrain))
                        (maxtileHtrain   ( getplayablemax humantiles humantrain (list -1 -1)))
                        (maxtileCtrain   ( getplayablemax humantiles computertrain (list -1 -1)))
                        (maxtileMtrain   ( getplayablemax humantiles mexicantrain (list -1 -1)))
                        (Ctrainmarked  (Computertrainmarked markerslist) )   
                        (tiletoplay (second (getmaxTiletrain maxtileCtrain maxtileHtrain maxtileMtrain Ctrainmarked)))
                        (Htrainorphan  (Isorphantrain humantrain))
                        (Mtrainorphan  (Isorphantrain mexicantrain))
                        (Ctrainorphan  (Isorphantrain computertrain))
                        (validtileHtrain    (tilenotnull maxtileHtrain))
                        (validtileCtrain    (tilenotnull maxtileCtrain))
                        (validtileMtrain    (tilenotnull maxtileMtrain))

                    )
                    (cond 
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
                            ( DisplayMaxtile humantiles computertiles boneyardtiles humantrain computertrain mexicantrain roundnumber humannext markerslist gamescores)
                        )
                    )
                )
               
            )
            ((eq (playabletiletrains humantiles humantrain computertrain mexicantrain (Computertrainmarked markerslist)) t)
                (DisplayMaxtile humantiles computertiles boneyardtiles humantrain computertrain mexicantrain roundnumber humannext markerslist gamescores)
            )
            (t
                (print "Human doesnot have any valid tiles to play. Please pick one from boneyard and continue")
            )
        )
    )

)

(defun Displayhelpmsg (train tile reason)

    (print "Play tile:")
    (princ tile)
    (princ " to " )
    (princ train)
    (princ "train as")
    (princ reason)

)

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
                        (let* (
                                (tiletoplay (getplayablemax computertiles  humantrain (list -1 -1)))
                                (tilenumber ( getTilelocation (length computertiles) computertiles tiletoplay)) 
                                (traintoplay (Gettrain "H" humantrain computertrain mexicantrain ))  
                            )
                            (playtiletotrain t (+ tilenumber 1) traintoplay (list tiletoplay) humantiles computertiles boneyardtiles
                                humantrain computertrain mexicantrain roundnumber (GetengineTile roundnumber) "H" humannext markerslist gamescores)
                        
                        )
                        
                    )
                    ((and Iscomputerorphan (playabletileexists computertiles computertrain))
                        ;computer train is an orphan train and tiles can be played here.
                        (print "Since the computer train was orphan double train.")
                        (let* (
                                (tiletoplay (getplayablemax computertiles  computertrain (list -1 -1)))
                                (tilenumber ( getTilelocation (length computertiles) computertiles tiletoplay)) 
                                (traintoplay (Gettrain "C" humantrain computertrain mexicantrain ))  
                            )
                            (playtiletotrain t (+ tilenumber 1) traintoplay (list tiletoplay) humantiles computertiles boneyardtiles
                                humantrain computertrain mexicantrain roundnumber (GetengineTile roundnumber) "C" humannext markerslist gamescores)
                        
                        )
                    )
                    ((and Ismexicanorphan (playabletileexists computertiles mexicantrain))
                        ;;computer train is an orphan train and tiles can be played here.  
                        (print "Since the mexican train was orphan double train.")
                        (let* (
                                (tiletoplay (getplayablemax computertiles  mexicantrain (list -1 -1)))
                                (tilenumber ( getTilelocation (length computertiles) computertiles tiletoplay)) 
                                (traintoplay (Gettrain "M" humantrain computertrain mexicantrain ))  
                            )
                            (playtiletotrain t (+ tilenumber 1) traintoplay (list tiletoplay) humantiles computertiles boneyardtiles
                                humantrain computertrain mexicantrain roundnumber (GetengineTile roundnumber) "M" humannext markerslist gamescores)
                        
                        )
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
                (let*
                    (
                        (tiletoplay (getplayablemax computertiles  mexicantrain (list -1 -1)))
                        (tilenumber ( getTilelocation (length computertiles) computertiles tiletoplay))
                        (traintoplay (Gettrain "M" humantrain computertrain mexicantrain ))
                    )
                    (playtiletotrain t (+ tilenumber 1) traintoplay (list tiletoplay) humantiles computertiles boneyardtiles
                                humantrain computertrain mexicantrain roundnumber (GetengineTile roundnumber) "M" t markerslist gamescores) 
                
                )
                
                ;(playround humantiles computertiles updatedboneyard humantrain updatedcomptrain mexicantrain roundnumber humannext updatedmarker gamescores)
            )
            ;must play a double train in case their is a valid double train to play
            ((eq (DoubletileValidTrains computertiles computertrain humantrain mexicantrain (Humantrainmarked markerslist ))   t )
                (print "Since the double tile is playable computer will try to make orphan double train.")
                (cond
                    ((and (eq (playabledoubleinTrain computertiles humantrain ) t) (eq (Humantrainmarked markerslist) t))
                        (let*
                            (
                                (tiletoplay (getplayabledoubletile computertiles humantrain))
                                (tilenumber ( getTilelocation (length computertiles) computertiles tiletoplay))
                                (traintoplay (Gettrain "H" humantrain computertrain mexicantrain ))
                            )
                            (playtiletotrain t (+ tilenumber 1) traintoplay (list tiletoplay) humantiles computertiles boneyardtiles
                                        humantrain computertrain mexicantrain roundnumber (GetengineTile roundnumber) "H" humannext markerslist gamescores) 
                        
                        )   
                    )
                    ((eq (playabledoubleinTrain computertiles computertrain ) t)
                        (let*
                            (
                                (tiletoplay (getplayabledoubletile computertiles computertrain))
                                (tilenumber ( getTilelocation (length computertiles) computertiles tiletoplay))
                                (traintoplay (Gettrain "C" humantrain computertrain mexicantrain ))
                            )
                            (playtiletotrain t (+ tilenumber 1) traintoplay (list tiletoplay) humantiles computertiles boneyardtiles
                                        humantrain computertrain mexicantrain roundnumber (GetengineTile roundnumber) "C" humannext markerslist gamescores) 
                        
                        )                                        
                    )
                    ((eq (playabledoubleinTrain computertiles mexicantrain ) t)
                        (let*
                            (
                                (tiletoplay (getplayabledoubletile computertiles mexicantrain))
                                (tilenumber ( getTilelocation (length computertiles) computertiles tiletoplay))
                                (traintoplay (Gettrain "M" humantrain computertrain mexicantrain ))
                            )
                            (playtiletotrain t (+ tilenumber 1) traintoplay (list tiletoplay) humantiles computertiles boneyardtiles
                                        humantrain computertrain mexicantrain roundnumber (GetengineTile roundnumber) "M" humannext markerslist gamescores) 
                        
                        )   
                    )
                    (T
                        (print "This wasnot expected! There has been error somewhere.")
                    )
                
                )


            )
            ;try to play on a train  that helps for creating orphan double train..
            ((and (> (getcontinousturns markerslist) 0 ) (eq (playabletiletrains computertiles computertrain humantrain mexicantrain (Humantrainmarked markerslist)) t))
                
                (let* 
                    (
                        (Htrainplayable  (playabletileexists computertiles humantrain))
                        (Ctrainplayable  (playabletileexists computertiles computertrain))
                        (Mtrainplayable  (playabletileexists computertiles mexicantrain))
                        (maxtileHtrain   ( getplayablemax computertiles humantrain (list -1 -1)))
                        (maxtileCtrain   ( getplayablemax computertiles computertrain (list -1 -1)))
                        (maxtileMtrain   ( getplayablemax computertiles mexicantrain (list -1 -1)))
                        (Htrainmarked  (Humantrainmarked markerslist) )
                        (maxtrain (first (getmaxTiletrain maxtileHtrain maxtileCtrain maxtileMtrain Htrainmarked)))
                        (tiletoplay (second (getmaxTiletrain  maxtileHtrain maxtileCtrain maxtileMtrain Htrainmarked)))
                        (tilenumber ( getTilelocation (length computertiles) computertiles tiletoplay))
                        (traintoplay (Gettrain maxtrain humantrain computertrain mexicantrain ))
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
        
        )
    
    )
)

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

(defun Automaticboneyardtotrain( humantiles computertiles boneyardtiles humantrain computertrain mexicantrain roundnumber humannext markerslist gamescores)
    (Print "Boneyard Tile: ")
    (Princ (first boneyardtiles))
    (cond
        ((and (eq (checktrain humantrain (list (first boneyardtiles)) ) t)  (eq (Humantrainmarked markerslist) t))
            ;needs to play boneyard tile to human train
            (Print "Computer picked the boneyard tile and placed it to the human train")
            (playround  humantiles computertiles (rest boneyardtiles) (addTiletoEnd humantrain (list (first boneyardtiles)) )  
                computertrain mexicantrain roundnumber humannext markerslist gamescores  )
        )
         ((eq (checktrain mexicantrain (list (first boneyardtiles)) ) t)
            ;needs to play boneyard tile to mexican train
            (Print "Computer picked the boneyard tile and placed it to the mexican train")
            (playround  humantiles computertiles (rest boneyardtiles) humantrain 
                computertrain (addTiletoEnd mexicantrain (list(first boneyardtiles)) )  roundnumber humannext markerslist gamescores  )
        )
       ((eq (checktrain computertrain (list (first boneyardtiles)) ) t)
            ;needs to play boneyard tile to computer train
            (Print "Computer picked the boneyard tile and placed it to the computer train")
            (playround  humantiles computertiles (rest boneyardtiles) humantrain 
                (addTiletoEnd computertrain (list(first boneyardtiles)) ) mexicantrain  roundnumber humannext markerslist gamescores  )
        )
        (T
            (print "Since the boneyard tile is not playable to any of the trains! Tile placed to computer's list of tiles.")
            (let* 
                (
                    (updated_markers (setcomputertrainmarker markerslist t))
                ) 
                (playround  humantiles (addTiletoEnd computertiles (list (first boneyardtiles))) (rest boneyardtiles) humantrain 
                computertrain mexicantrain  roundnumber humannext updated_markers gamescores  )
            )
            
        )
    )
)


(defun ForwardtiletraintoPlay(tiletoplay trainletter humantrain computertrain mexicantrain humantiles computertiles boneyardtiles roundnumber 
            humannext markerslist gamescores)

    (let*
        (
            (tilenumber ( getTilelocation (length computertiles) computertiles tiletoplay))
            (traintoplay (Gettrain trainletter humantrain computertrain mexicantrain ))
        )
        (playtiletotrain t (+ tilenumber 1) traintoplay (list tiletoplay) humantiles computertiles boneyardtiles
                    humantrain computertrain mexicantrain roundnumber (GetengineTile roundnumber) trainletter humannext markerslist gamescores) 
    
    ) 



)


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

(Defun displayforceorphanmsg(tile train)
    (print "Computer player plays the tile: ")
    (princ tile)
    (princ " to the: ")
    (princ train)
    (princ "train as it forces opponent to play orphan double train")

)

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
        
        
        (print "The largest tile possible was:")  
        (princ tiletoplay)
        (Print "So playing the tile on:")
        (princ maxtrain)
        (princ " train")

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

;this function gives back the train where maxtile can be played and the tile that is max.
(defun getmaxTiletrain(maxtileopponenttrain maxtileselftrain maxtileMtrain opponenttrainmarked)  

    (cond 
        ((and (and (eq opponenttrainmarked t) (>= (sum maxtileopponenttrain) (sum maxtileselftrain))) (>= (sum maxtileopponenttrain) (sum maxtileMtrain))) 
            (cons 'H (cons maxtileopponenttrain ()))
        )
        ((and ( >= (sum maxtileselftrain) (sum maxtileopponenttrain))  (>= (sum maxtileselftrain) (sum maxtileMtrain)))
            (cons 'C (cons maxtileselftrain ()))
        )
        ((and ( >= (sum maxtileMtrain) (sum maxtileopponenttrain))  (>= (sum maxtileMtrain) (sum maxtileselftrain))) 
            (cons 'M (cons maxtileMtrain ()))
        )
        (( >= (sum maxtileMtrain) (sum maxtileselftrain))   
            (cons 'M (cons maxtileMtrain ()))
        )
        (t   
            (cons 'C (cons maxtileselftrain ()))
        )
    )
)


;this function gives back the train where maxtile can be played and the tile that is max.
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
        )
    
    )
)

;checks if there is a double tile that can be played in any of the valid trains.
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


;this gives true or false value based on whether there is a double tile that can be played on the given train.
(defun playabledoubleinTrain(tiles train)
   (let*
        (   
            ;first tile of tiles list
            (tile_sidea (first (first tiles)) )
            (tile_sideb (second (first tiles)))  
            (trainend  (first (reverse train))) 
            (tiletomatch (second trainend))
        )
      
        ;(print tiletomatch)
        (cond
            ((eq (length tiles) 0) 
                nil
            )
            ((and (eq tile_sidea tiletomatch) ( eq tile_sideb tile_sidea)) 
                ;(print (first tiles))
                ;(print trainend)
                t
            )
            (t 
                ;(print (second (first tiles))) 
                (playabledoubleinTrain (rest tiles) train)    
            )
        
        )
   )
) 

;this gives a playable double tile in case it exists.
(defun getplayabledoubletile(tiles train)
   (let*
        (   
            ;first tile of tiles list
            (tile_sidea (first (first tiles)) )
            (tile_sideb (second (first tiles)))  
            (trainend  (first (reverse train))) 
            (tiletomatch (second trainend))
        )
      
        ;(print tiletomatch)
        (cond
            ((eq (length tiles) 0) 
                nil
            )
            ((and (eq tile_sidea tiletomatch) ( eq tile_sideb tile_sidea)) 
                ;(print (first tiles))
                ;(print trainend)
                (cons tile_sidea (cons tile_sideb ()))
            )
            (t 
                ;(print (second (first tiles))) 
                (getplayabledoubletile (rest tiles) train)    
            )
        
        )
   )
) 


;boneyard tile is placed on the orphan double train if possible else added to the players list.
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
                )
                
            )
        
        )
    )
)

(defun playabletileexists(tiles train)
   (let*
        (   
            ;first tile of tiles list
            (tile_sidea (first (first tiles)) )
            (tile_sideb (second (first tiles)))  
            (trainend  (first (reverse train))) 
            (tiletomatch (second trainend))
        )
      
        ;(print tiletomatch)
        (cond
            ((eq (length tiles) 0) 
                nil
            )
            ((or (eq tile_sidea tiletomatch) ( eq tile_sideb tiletomatch)) 
                ;(print (first tiles))
                ;(print trainend)
                t
            )
            (t 
                ;(print (second (first tiles))) 
                (playabletileexists (rest tiles) train)    
            )
        
        )
   )
) 

(defun sum(tile)

   (+ (first tile) (second tile))

)


(defun maxsum(tilea tileb)

    (cond( (>  (+ (first tilea ) (second tilea))  (+ (first tileb ) (second tileb)))
            tilea
        )
        (T
            tileb
        )
    
    )
)

(defun getplayablemax (tiles train finaltile)

    (let*
            (   
                ;first tile of tiles list
                (tile_sidea (first (first tiles)) )
                (tile_sideb (second (first tiles))) 
                (trainend   (first (reverse train))  )  
                (tiletomatch (second trainend))
            )

        
            ;(print tiletomatch)
            (cond
                ((eq (length tiles) 0) 
                    ;(print finaltile)
                    finaltile
                )
                ((or (eq tile_sidea tiletomatch) ( eq tile_sideb tiletomatch)) 
                    (getplayablemax (rest tiles) train (maxsum (first tiles) finaltile))
                )
                (t 
                    ;(print (second (first tiles))) 
                    (getplayablemax (rest tiles) train finaltile)    
                )
            )
    )
)

(defun trainstarted(train)
    (cond ((= (length train) 1 )
            t    
        )
        (t 
            nil
        )
    )
)

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

(defun nextplayername(humannext)
    (cond((eq humannext t)
            'HUMAN
        ) 
        (T
            'COMPUTER
        )  
    )   

)

(defun addhumanmarker(tiles markerslist)

    (cond 
        ((equal (Humantrainmarked markerslist) t)
            (addTiletoEnd tiles (cons 'M ()) )
        )
        (T
            tiles
        )
    
    )

)

(defun CheckOrphanandPlay(trainisplayable tileinput traintoplay tile humantiles computertiles boneyardtiles
                humantrain computertrain mexicantrain roundnumber enginetile traininput humannext markerslist orphanexists orphanplayed gamescores)

        (cond 
            ((and (equal orphanexists t) (equal orphanplayed nil))
                (print "Sorry you must play the orphan double train!")
                (playround humantiles computertiles boneyardtiles humantrain computertrain mexicantrain roundnumber (reverseturn humannext) markerslist gamescores)
            )
            (t 
                (playtiletotrain trainisplayable tileinput traintoplay tile humantiles computertiles boneyardtiles
                            humantrain computertrain mexicantrain roundnumber enginetile traininput humannext markerslist gamescores)
            )
        )
)
;the main goal of this function is to get the train from the tiles 
(defun Gettrain(traininput humantrain computertrain mexicantrain )
    (cond ((string-equal traininput "H") humantrain )
            ((string-equal traininput "C") computertrain)
            ((string-equal traininput "M") mexicantrain)
            (t (print "Invalid train input entered!")
                nil
            )   
    )               
)

;this function will take all the parameters as input and in case train is playble call playround with modified value
;if not playable it 
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
                )
            )
            )
        )
        ;user needs to pick a boneyard tile or replay the move
        ;(reverseturn humannext) helps to make sure same player plays again instead of alternating the turns as move is not completed.
        (t (print "Tile you chose is not playable on the given train! Please play again")
           (playround humantiles computertiles boneyardtiles humantrain computertrain mexicantrain roundnumber (reverseturn humannext) markerslist gamescores) 
        )            
    )
)

; this function helps to allow to play again in case of the double tile by modifying human next value.
(defun RepeatPlayer( doubletileplayed humannext )

    (cond
        ((equal doubletileplayed t)
            (cond
                ((equal humannext t)
                    nil
                )
                (t 
                    t
                )
            )
        )
        (t 
            humannext
        )
    
    )
)

; this change the current state of the human next to opposite value which helps the players to play their turn again.
(defun reverseturn(humannext)
    (cond
        ((eq humannext t)
            nil
        )
        (t 
           t 
        )
    )
)

;returns the tile of the current player
(defun Currentplayertiles(humannext humantiles computertiles)

    (cond
        ((eq humannext t)
            computertiles
        )
        (t humantiles) 
    )
) 

;this function should update the parameters when the move is made and continue calling playround for playing another
;move.This is a helper function which modifies the humantiles and 
(defun updateplayertiles(updatedplayertiles updatedplayedtrain  humantiles computertiles boneyardtiles
                humantrain computertrain mexicantrain roundnumber enginetile traininput humannext markerslist modified_humannext gamescores ) 

    ;this condition should determine what trains value to change whether to change user tiles or the computer tiles for the next part.           
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

( defun Displaynextplayer (player)
    
    (TERPRI)
    (princ player)
    (princ "'s turn next") 
    (cond
        ((string-equal player "computer")
            (print "Please enter (C) to continue and (S) to serialize and quit.")
        )
        (T
         (print "please enter as per your choice:(play-HTrain(H) , PlayCTrain(C) ,PlayMTrain(M) ,Pick-Boneyard(B),Serialize(S) or Assistance(A))")
        )
    )
    (terpri)
    (princ ">>")
) 
;this function deals with the user view so that state of the game is correctly displayed to the user.
(defun display(humantiles computertiles boneyardtiles humantrain computertrain mexicantrain roundnumber enginetile humannext markerslist gamescores)

    
    (print "-----------------------------------------------------------------------------------------------")
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


    (cond
        ((<= (length humantrain) 15)
                (format T "~80@a" "")
                (displaydoubletraintiles (rest humantrain) 1)
                (TERPRI)
                (format T "~80@a" "Humantrain-->")
                (displaytraintiles (rest humantrain) 1)
                (cond
                    ((equal (Humantrainmarked markerslist) t)
                        (princ "M")
                    )
                )
                (TERPRI)
                (format T "~80@a" "")
                (displaydoubletraintiles (rest humantrain) 1)
                (TERPRI)
        
        )
        (T
                (format T "~80@a" "Humantrain-->")
                (displaydoubletraintiles (sublist humantrain 1 15) 1)
                (TERPRI)
                (format T "~80@a" "")
                (displaytraintiles (sublist humantrain 1 15) 1)
                (TERPRI)
                (format T "~80@a" "")
                (displaydoubletraintiles (sublist humantrain 1 15) 1)
                (TERPRI)
                ;-----remaining part of the tiles
                (displaydoubletraintiles (sublist humantrain 16 nil) 1)
                (TERPRI)
                (displaytraintiles (sublist humantrain 16 nil) 1)
                (TERPRI)
                (cond
                    ((equal (Humantrainmarked markerslist) t)
                        (princ "M")
                    )
                )
                (displaydoubletraintiles (sublist humantrain 16 nil) 1)
                (TERPRI)
        
        )
    
    )


   
    ;--------------------------------------------------------------------------------------

    (format T "~80@a" "")
    (princ (first (first enginetile)))
    (TERPRI)
    
    (format T "~81@a" "EngineTile-->|")
    (TERPRI)
    
    (format T "~80@a" "")
    (princ (second (first enginetile)))
    (TERPRI)
    ;-----------------------------computer train--------------------------------------------------
    (printspaces (- 82 (* (length computertrain) 6) )) 
    (displaydoubletraintiles (reversecomputertrain (reverse (rest computertrain)) (list )) 1)
   
    
    (TERPRI)
    (printspaces (- 82 (* (length computertrain) 6) )) 
    (cond
            ((equal (Computertrainmarked markerslist) t)
                (princ "M")
            )
    )
    (displaytraintiles (reversecomputertrain (reverse (rest computertrain)) (list )) 1)
    (princ " <- computer")
    (TERPRI)
    (printspaces (- 82 (* (length computertrain) 6) )) 
    (displaydoubletraintiles (reversecomputertrain (reverse (rest computertrain)) (list )) 1)
   
    (TERPRI)
    ;--------------------------------------------------------------------------------------------

    (cond
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
        
        )
    
    )
 
    ;---------------------------------------------------------------------------------
    (print "Computer Tiles: ")
    (TERPRI)
    (displaytiles computertiles 1)
    (TERPRI) (TERPRI) (TERPRI)
    (print "boneyard Tiles: ")
    (TERPRI)
    (displaytiles boneyardtiles 1)
    (print "-----------------------------------------------------------------------------------------------")

)

(defun printspaces(times) 
    (cond
        ((<= times 1)
            (princ " ")
        )
        (T
            (princ " ")
            (printspaces (- times 1))
        )
    )

)

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

;;this function checks if the last tile of the train is a match with the tile the user wants to add with
(defun checktrain(train tile) 

    ;(print "checktrain is working")
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
;this function rotates the sides of the train tiles so that it is easier to print the tiles correctly
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
        )
    )    

)

;removes the tile of a given number from the tiles list 
(defun removeTile(tile_number tiles_list)
  (cond 
    ((< tile_number 1)
      tiles_list)
    ((= tile_number 1)
      (cdr tiles_list))
    (t
      (cons (car tiles_list) (removeTile (- tile_number 1) (cdr tiles_list))))))


;;adds the tiles to the end of the train
(defun addTiletoEnd(tiles_list tile)
    (append tiles_list tile)
)

;this function should move the tiles from a boneyard to player tiles list if picked from boneyard.
;train as well as tiles of the current players are 
(defun boneyardtoplayertiles(humantiles computertiles boneyardtiles  humantrain computertrain mexicantrain roundnumber enginetile humannext markerslist gamescores)
    
    (cond ((equal (length boneyardtiles) 0)
            (print "There were no tiles to pick from boneyard so player train is marked.")
            
            (cond
                ;current player is huma
                ((equal humannext nil)
                    (let 
                        (
                            (updated_marker (sethumantrainmarker markerslist t) )
                        )
                        (playround humantiles computertiles boneyardtiles humantrain computertrain mexicantrain roundnumber humannext updated_marker gamescores)
                    )
                )
                (T
                    (let 
                        (
                            (updated_marker (setcomputertrainmarker markerslist t) )
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
                    ;(doubletile (Checkdoubletile tiletoadd))
                    ;modified humannext helps to determine if the player is to be called again or not based on the double tile.
                    ;(modified_humannext (RepeatPlayer doubletile humannext))
                    ;this statement help to set the number of turn same players has played
                    ;value stored inside markerslist (t t 0)--> 3rd param
                    ;(markcontinousturn (setcontinousturnplayed markerslist doubletile))
                    
                )

                ;--------------------output of the boneyard tile---------------
                (princ "Boneyard tile is:")
                (princ tiletoadd)
                ;----------------------------------------------------------------
                (cond
                    ((eq humannext t)
                        ;if human player is next tiles of the computer players are modified and new move is initiated for the current round.
                        ;boneyard is played by computer train
                        ;condition checks if the boneyard tile is playable or not.
                        (cond
                            ((eq (boneyardtileplayable tiletoadd humantrain computertrain mexicantrain humannext markerslist) t)
                            
                                (print "boneyard tile playable so try to play it.")
                                ;function that takes input from the user to move to the required train is placed here.
                                (Playboneyardtile humantiles computertiles boneyardtiles humantrain computertrain mexicantrain roundnumber enginetile humannext markerslist tiletoadd gamescores)
                            )
                            (t 
                                (let* 
                                    ; marker format (nil nil 0) --(human train marker, computer train marker, number of moves played before in same turn)
                                    ;marker set to true
                                    ((added_marker (setcomputertrainmarker markerslist t)))
                                    (print "Boneyard tile is not playable so computer players turn is skipped and tile is added to the list.")
                                    (playround humantiles updatedtiles updatedboneyard humantrain computertrain mexicantrain roundnumber humannext added_marker gamescores)
                                )
                                
                            )
                        
                        )
                            
                    )
                    (t 
                        
                        ;human tiles need to be updated in this case.
                        ;boneyard is played from human player
                        (cond
                            ;in case the boneyard tile is playable give user choice of train to play
                            ;else continue the round 
                            ((eq (boneyardtileplayable tiletoadd humantrain computertrain mexicantrain humannext markerslist) t)
                            
                                ;(print "boneyard tile playable so try to play it.")
                                ;function that takes input from the user to move to the required train is placed here.
                                (Playboneyardtile humantiles computertiles boneyardtiles humantrain computertrain mexicantrain roundnumber enginetile humannext markerslist tiletoadd gamescores) 
                            )
                            (t 
                                (let*
                                    ;marked the human train to true as boneyard tile was not playable.
                                    ((added_marker (sethumantrainmarker markerslist t)))
                                    (print "Boneyard tile is not playable so human player turn is skipped and tile is added to list.Marker added.")
                                    (playround updatedtiles computertiles updatedboneyard humantrain computertrain mexicantrain roundnumber humannext added_marker gamescores)
                                )
                                
                            )
                        
                        )


                    ) 
                )
                
            )
        )
    )
    
)

( defun boneyardtileplayable( tile humantrain computertrain mexicantrain humannext markerslist)
    
    (let*
        (
            (orphanexists (Orphandoubleexists humantrain computertrain mexicantrain markerslist))
        )
        
        (cond (
            (equal orphanexists t)
                (cond
                    ( (and (equal (Isorphantrain humantrain) t )  (equal (checktrain humantrain tile ) t ) ) 
                        t
                    
                    )
                    ( (and (eq (Isorphantrain computertrain) t )  (equal (checktrain computertrain tile ) t)  )
                        t
                    
                    )
                    ( (and (eq (Isorphantrain mexicantrain) t )  (equal (checktrain mexicantrain tile ) t)  )
                        t
                    
                    )
                    (t
                        nil
                    )
                )
            )
            (t 
                    
                (cond
                    ;current player is human
                    ((eq humannext nil)

                        (cond 
                            ((eq (checktrain humantrain tile) t) 
                                ;playable on human train
                                t   
                            )
                            ((eq (checktrain mexicantrain tile) t) 
                                ;playable on mexican train
                                t   
                            )
                            (( and (eq (checktrain computertrain tile) t) (eq (Computertrainmarked markerslist) t) )
                                ;playable on computer train because of the marker 
                                t   
                            )
                            (t 
                                ;not playable on anytrain
                                nil
                            ) 
                        )
                    )

                    ;current player is computer
                    ((eq humannext t)

                        (cond 
                            ((eq (checktrain computertrain tile) t) 
                                ;playable on computer train"
                                t   
                            )
                            ((eq (checktrain mexicantrain tile) t) 
                                ;playable on mexican train
                                t   
                            )
                            (( and (eq (checktrain humantrain tile) t) (eq (Humantrainmarked markerslist) t) )
                                ;playable on human train because of marker
                                t   
                            )
                            (t 
                                ;not playable
                                nil
                            )
                        
                        )
                    
                    )
                    (t (print "Invalid input for next player!!!1"))
                )
                    
            )
        )
    )
            

)

;(nil nil )--> this is the format of the markerslist
( defun sethumantrainmarker (markerlist value)
    (cons value (rest markerlist))
)

;sets computer marker to given value | T or nil
( defun setcomputertrainmarker (markerlist value)
    (cons (first markerlist) (cons value (list (second (rest markerlist)))))
)

;this sets the continous number of turns played by the current player if the turn was repeated
(defun getcontinousturns(markerlist)

    (second (rest markerlist))

)

(defun Humantrainmarked(markerlist)
    (cond
    ((eq (first markerlist) T)
        t
    )
    (t nil)
    )

)

(defun Computertrainmarked(markerlist)
    (cond
    ((eq (second markerlist) T)
        t
    )
    (t nil)
    )
)

;set continous turn played increases the number if double tile is played if non double tile is played it clears the
;value to zero
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
        )
    )
)

(defun displaydoublemsg(tile)
    (cond
        ((eq (Checkdoubletile tile) t)
            (print "Since double tile was played one more play awarded to the same player.")
        )
    )
    
)


;this function helps to move a latest boneyard tile from a user pile to one of the trains
;given that atleast one of the train is playable.

( defun Playboneyardtile ( humantiles computertiles boneyardtiles humantrain computertrain mexicantrain roundnumber enginetile humannext markerslist
    tiletoadd gamescores)
    (print "Please enter the train where you want to place the playable boneyard tile>>")

    (let*
        (   (traininput (readtrain))
            (traintoplay (Gettrain traininput humantrain computertrain mexicantrain ))
            (tileplayable (checktrain traintoplay tiletoadd))
            (rotatedtile (rotatetile traintoplay tiletoadd))
            (updatedboneyard (rest boneyardtiles))
            ;---------------------------------------------------------------------------------------------------------------
            (doubletile (Checkdoubletile tiletoadd))
            ;modified humannext helps to determine if the player is to be called again or not based on the double tile.
            (modified_humannext (RepeatPlayer doubletile humannext))
            ;this statement help to set the number of turn same players has played
            ;value stored inside markerslist (t t 0)--> 3rd param
            (markcontinousturn (setcontinousturnplayed markerslist doubletile))
            ;------------------------------------------------------------------------------------------------------------------
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
                    ;in this case current player is human
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
                    (t 
                        ;current player is computer
                        ;this will be handled as computer player
                        ;computer wants to put the boneyard tile to computer train
                        (cond 
                            ((string-equal traininput "C")
                                (let* 
                                    (
                                        (updatedcomputertrain (addTiletoEnd computertrain rotatedtile))
                                        (updated_markers (Erasemarker "C" "C" markerslist ))
                                    )
                                    ;--------------------expressions below this---------------------------------------
                                    (print "Boneyard tile has been played to the computer train")
                                    ( displaydoublemsg tiletoadd)
                                    (playround humantiles computertiles updatedboneyard humantrain updatedcomputertrain mexicantrain roundnumber  modified_humannext updated_markers gamescores)
                            ))
                            ;computer player wants to put the boneyard to to mexican train
                            ((string-equal traininput "M")
                                (let* (
                                        (updatedmexicantrain (addTiletoEnd mexicantrain rotatedtile ))
                                    )
                                    ;--------------------expressions below this---------------------------------------
                                    (print "Boneyard tile has been played to the mexican train")
                                    ( displaydoublemsg tiletoadd)
                                    (playround humantiles computertiles updatedboneyard humantrain computertrain updatedmexicantrain roundnumber  modified_humannext markerslist gamescores)
                                ))
                            ;computer player wants to put the boneyard tile to the human train.
                            ;in this case checks if the computer train is marked or not.
                            ( (or  (and (string-equal traininput "H") (eq (Humantrainmarked markerslist) t)) (Isorphantrain humantrain))
                                (let* (
                                        (updatedhumantrain (addTiletoEnd humantrain rotatedtile ))
                                        (updated_markers (Erasemarker "C" "H" markerslist ))
                                    )   
                                    ;--------------------expressions below this---------------------------------------
                                    (print "Boneyard tile has been played to the human train")
                                    ( displaydoublemsg tiletoadd)
                                    (playround humantiles computertiles updatedboneyard updatedhumantrain computertrain mexicantrain roundnumber  modified_humannext updated_markers gamescores)
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

(defun GetengineTile(roundnumber) 
    (let*(
            (modround (mod roundnumber 10))
            (value (- 10 modround))
        )
        (list (cons value (cons value ())))
    ) 
)

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

(defun removeEnginetile(tile_position shuffleddeck)
  (COND 
    ((< tile_position 1)
      shuffleddeck)
    ((= tile_position 1)
      (REST shuffleddeck))
    (t
      (CONS (FIRST shuffleddeck) (removeEnginetile (- tile_position 1) (REST shuffleddeck))))
    )
)

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
                    )
                
                )



            )
        )
            
    )
)

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
            )
        
        )
        
    )

)

;this gives the total score when the remaining tiles of the player is given as input.
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
                ;(print sum)
                ;(print newtotal)
                (totalscoreoftrain (rest vect) newtotal)
            )
        )
    )    
)

;file read and write help taken from https://www.tutorialspoint.com/lisp/lisp_file_io.htm
(defun loadsavedfile()
  (print "Please enter the name of the file with .lisp: ")
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
    )
)

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

(defun removehumanmarker(vect) 
    (let*
    
        (
            (possiblehumanmarker (elt vect (- (length vect) 1)))
        )
         
    (cond 
        ((listp possiblehumanmarker )
            ;marker doesnot exists
            vect
        )
        (       
            ;marker  exists
            (removeTile (length vect) vect) 
        ))))

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


;this function returns if the vect has a marker at the begining of the list
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

;this function helps to reverse the computer tiles inside the individual tiles.
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

(mexican-train)



