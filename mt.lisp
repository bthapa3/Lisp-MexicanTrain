;;;starts the game by taking unput on whether to start a game or load it
(defun mexican-train()
    ( print "Please select one of the options")
    ( print "(1) to start a new game")
    ( print "(2) to load a saved game from a file")
    (LET ((useroption ( readnum 1 2)))  
        (print useroption) 
        (cond ((= useroption 1)
            ;starting a new game if the user enter 1
            (startgame useroption))
            ;reading a game from file if the user presses 2
            (t (startgame useroption ))
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
    (LET ((userInput (READ)))
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
        ((or (or (or (string-equal userInput "M") (string-equal userInput "C")) (string-equal userInput "H")) (string-equal userInput "B") )    
            userInput)
        (t 
            (print "Invalid train entered! Choose among Human(H) computer(C) Mexican(M) or Boneyard(B)")
            (print ">>")
            (readtrain))
    )
)

;;;this function checks the useroption and tries to start a new game
;;;by creating a new tiles deck or loading from the deck
(defun startgame (useroption)
    (cond 
       ( (= useroption 1)
            (print "User decided to start a new game") 
            (print "----------------------------------------------------------------------------------")
            ;note I can also pass the deck to shuffle deck directly from here instead of using let
            ; and that is what lisp programming is all about.
            ;generating a deck of 55 tiles and shuffling it
            ;need to do a toss here in case of already played game

            (let* ( (deck (shuffledeck (generatedeck 9 9 )))
                    (enginetile (GetengineTile 1))
                    (enginetile_pos (getTilelocation 54 deck (car enginetile)))
                    (finaldeck ( removeEnginetile (+ enginetile_pos 1) deck))
                    (humantiles (sublist finaldeck 0 16)) 
                    (computertiles (sublist finaldeck 16 16))
                    (boneyardtiles (sublist finaldeck 32 nil))
                )
                (startround humantiles computertiles boneyardtiles 1 t 0 0) 
            )
            
       )
       ( (= useroption 2)
            ;need to read a file here and start a game from here
            (print "User decided to load a saved game") 
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
(defun startround (humantiles computertiles boneyardtiles roundnumber humannext userscore computerscore)
    
   ;Here all the trains and engine have tile 9-9 saved as this is a starting round
    (playround humantiles computertiles boneyardtiles (list (list 9 9)) (list (list 9 9)) (list (list 9 9))  1 (GetengineTile roundnumber) t (list nil nil))
    ;changes for new round to be made here.


)

;this function should be used recursively to play rounds
(defun playround(humantiles computertiles boneyardtiles humantrain computertrain mexicantrain roundnumber enginetile humannext markerslist) 
    (print "press any key to continue")
    (LET ((userInput (READ))))
    ( display humantiles computertiles boneyardtiles humantrain computertrain mexicantrain roundnumber enginetile humannext)
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
                        (boneyardtoplayertiles humantiles computertiles boneyardtiles humantrain computertrain mexicantrain roundnumber enginetile nil markerslist)
                    )
                    (t 
                        (let* (
                            (tileinput (readnum 1 (length humantiles)))
                            ;tile is the actual tile(x y) that user chose to play to the desired train
                            (tile (sublist humantiles (- tileinput 1) 1 ))
                            ;train to play has the tiles of the train that user wants to place the tile to 
                            (traintoplay (Gettrain traininput humantrain computertrain mexicantrain ))
                            ;trainisplayable holds if the user train is playable for the tileinput
                            (trainisplayable (checktrain traintoplay tile)))
                            ;;--------------variables above---------------------------
                            ;once the tile is played to the train it should call itself recursively so that game continues
                            (playtiletotrain trainisplayable tileinput traintoplay tile humantiles computertiles boneyardtiles
                            humantrain computertrain mexicantrain roundnumber enginetile traininput nil markerslist)
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
                    (traininput (readtrain))  
                )
                ;----------------------------------expression below use the variable above-----------------
                (cond ((string-equal traininput "B")
                        ;(print "User picked up a tile from the boneyard tile added to the list.")
                        ;function to add tiles to user train goes here.
                        (boneyardtoplayertiles humantiles computertiles boneyardtiles humantrain computertrain mexicantrain roundnumber enginetile t markerslist)
                    )
                    (t 
                        (let* (
                            (tileinput (readnum 1 (length computertiles)))
                            ;tile is the actual tile(x y) that user chose to play to the desired train
                            (tile (sublist computertiles (- tileinput 1) 1 ))
                            ;train to play has the tiles of the train that user wants to place the tile to 
                            (traintoplay (Gettrain traininput humantrain computertrain mexicantrain ))
                            ;trainisplayable holds if the user train is playable for the tileinput
                            (trainisplayable (checktrain traintoplay tile))
                            )
                            ;;--------------variables above---------------------------
                            ;once the tile is played to the train it should call itself recursively so that game continues
                            (playtiletotrain trainisplayable tileinput traintoplay tile humantiles computertiles boneyardtiles
                            humantrain computertrain mexicantrain roundnumber enginetile traininput t markerslist)
                            ;this should be the last thing to run before this function exits.
                            ;( playround humantiles computertiles boneyardtiles humantrain computertrain mexicantrain roundnumber enginetile nil)
                            ;-------------------------end of expressions----------------------------------------------------

                        )
                    )
                )
   
            )
            
        )
    )
)

;the main goal of this function is to get the train from the tiles 
(defun Gettrain(traininput humantrain computertrain mexicantrain )
    (cond ((string-equal traininput "H") humantrain )
            ((string-equal traininput "C") computertrain)
            ((string-equal traininput "M") mexicantrain)
            (t (print "Invalid tile input sent here!"))
    )
)

;this function will take all the parameters as input and in case train is playble call playround with modified value
;if not playable it 
(defun playtiletotrain(trainisplayable tileinput traintoplay tile humantiles computertiles boneyardtiles
                humantrain computertrain mexicantrain roundnumber enginetile traininput humannext markerslist)

    (cond 
        ((eq trainisplayable t)
            
            ;these function help to remove the tiles from the usertiles and add to the chosen train
           
            (LET* 
                ;currentplayer tiles function gets the tile of the current user 
                ;removetile removes the given tile from the current user tiles
                (   (updatedplayertiles (removeTile tileinput (Currentplayertiles humannext humantiles computertiles)) )
                    (rotatedtile (rotatetile traintoplay tile) )
                    (updatedplayedtrain (addTiletoEnd traintoplay rotatedtile))
            
                )
            ;-------------------------------expression below this---------------------------------------------
            
            (cond
                ;if computer train is not marked and human player tries to play computer train.
                ( (and (and (eq humannext nil) (eq (Computertrainmarked markerslist) nil))  (string-equal traininput "C"))
                    (print "Sorry human player cannot play tile on umarked computer train")
                    (playround humantiles computertiles boneyardtiles humantrain computertrain mexicantrain roundnumber enginetile (reverseturn humannext) markerslist)
                )
                ;if human train is not marked and computer player tries to play human train.
                ( (and (and (eq humannext t) (eq (Humantrainmarked markerslist) nil))  (string-equal traininput "H"))
                    (print "Sorry computer player cannot play tile on umarked human train")
                    (playround humantiles computertiles boneyardtiles humantrain computertrain mexicantrain roundnumber enginetile (reverseturn humannext) markerslist)
                )
                (t 
                    ;following lines help to display user if the tile was placed succesfully
                    (princ tile)
                    (princ " tile was placed on the ")
                    (princ traininput)
                    (princ " train")
                    ( updateplayertiles updatedplayertiles updatedplayedtrain humantiles computertiles boneyardtiles
                    humantrain computertrain mexicantrain roundnumber enginetile traininput humannext markerslist)
                )
            )
            )
        )
        ;user needs to pick a boneyard tile or replay the move
        ;(reverseturn humannext) helps to make sure same player plays again instead of alternating the turns as move is not completed.
        (t (print "Tile you chose is not playable on the given train! Please play again")
           (playround humantiles computertiles boneyardtiles humantrain computertrain mexicantrain roundnumber enginetile (reverseturn humannext) markerslist) 
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
                humantrain computertrain mexicantrain roundnumber enginetile traininput humannext markerslist) 

    ;this condition should determine what trains value to change whether to change user tiles or the computer tiles for the next part.           
    (cond ((eq humannext nil)
            ;current player is human
            ;in this case humantiles will be replaced by updatedplayertiles
            (updateplayedtrain updatedplayedtrain updatedplayertiles computertiles boneyardtiles
               humantrain computertrain mexicantrain roundnumber enginetile traininput humannext markerslist)
        )
        (t 
            ;current player is computer
            ;in this case computer tile will be replaced by updatedplayertiles.
            (updateplayedtrain updatedplayedtrain humantiles updatedplayertiles boneyardtiles
              humantrain computertrain mexicantrain roundnumber enginetile traininput humannext markerslist)
        )      
    )
)


(defun updateplayedtrain(updatedtrain humantiles computertiles boneyardtiles
                humantrain computertrain mexicantrain roundnumber enginetile traininput humannext markerslist)

    (cond ((string-equal traininput "M")
            ;train to be updated is mexican
            ;this continues the current round by modifying the trains values
            ( playround humantiles computertiles boneyardtiles humantrain computertrain updatedtrain roundnumber enginetile humannext markerslist)

        )
        ((string-equal traininput "H")
            ;train to be updated is human 
            ( playround humantiles computertiles boneyardtiles updatedtrain computertrain mexicantrain roundnumber enginetile humannext markerslist)    
        )
        ((string-equal traininput "C")
            ;train to be updated is computer" 
            ( playround humantiles computertiles boneyardtiles humantrain updatedtrain mexicantrain roundnumber enginetile humannext markerslist)    
        )   
        (t (print "Invalid train played"))  
    )    
)


( defun Displaynextplayer (player)
    
    (TERPRI)
    (princ player)
    (princ "'s turn next please enter the train you want to play:(Human(H),Computer(C),Mexican(M),Boneyard(B))")
    (print ">>")
) 
;this function deals with the user view so that state of the game is correctly displayed to the user.
(defun display(humantiles computertiles boneyardtiles humantrain computertrain mexicantrain roundnumber enginetile humannext)

    (print "-----------------------------------------------------------------------------------------------")
    (print "Human Tiles: ")
    (TERPRI)
    (displaytiles humantiles 1 )
    (TERPRI) (TERPRI) (TERPRI)
    
    (print "Humantrain")
    (displaytiles humantrain 1)
    (TERPRI)

    (print "enginetile")
    (displaytiles enginetile 1)
    (TERPRI)

    (print "computertrain")
    (displaytiles computertrain 1)

    (TERPRI)
    (print "mexicantrain")
    (displaytiles mexicantrain 1)
    (TERPRI)
    
    (print "Computer Tiles: ")
    (TERPRI)
    (displaytiles computertiles 1)
    (TERPRI) (TERPRI) (TERPRI)
    (print "boneyard Tiles: ")
    (TERPRI)
    (displaytiles boneyardtiles 1)
    (print "-----------------------------------------------------------------------------------------------")

)
;
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
(defun boneyardtoplayertiles(humantiles computertiles boneyardtiles  humantrain computertrain mexicantrain roundnumber enginetile humannext markerslist)
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
            ((eq humannext t)
                ;if human player is next tiles of the computer players are modified and new move is initiated for the current round.
                ;boneyard is played by computer train
                ;condition checks if the boneyard tile is playable or not.
                (cond
                    ((eq (boneyardtileplayable tiletoadd humantrain computertrain mexicantrain humannext markerslist) t)
                    
                        (print "boneyard tile playable so try to play it.")
                        ;function that takes input from the user to move to the required train is placed here.
                        (Playboneyardtile humantiles computertiles boneyardtiles humantrain computertrain mexicantrain roundnumber enginetile humannext markerslist tiletoadd)
                    )
                    (t 
                        (print "Boneyard tile is not playable so computer players turn is skipped and tile is added to the list.")
                        (playround humantiles updatedtiles updatedboneyard humantrain computertrain mexicantrain roundnumber enginetile humannext markerslist)
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
                        (Playboneyardtile humantiles computertiles boneyardtiles humantrain computertrain mexicantrain roundnumber enginetile humannext markerslist tiletoadd)
                    )
                    (t 
                        (print "Boneyard tile is not playable so human player turn is skipped and tile is added to list.")
                        (playround updatedtiles computertiles updatedboneyard humantrain computertrain mexicantrain roundnumber enginetile humannext markerslist)
                    )
                
                )


            ) 
        )
         
    )
)

( defun boneyardtileplayable( tile humantrain computertrain mexicantrain humannext markerslist)
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

;(nil nil )--> this is the format of the markerslist
( defun sethumantrainmarker (markerlist value)
    (cons value (rest markerlist))
)

;sets computer marker to given value | T or nil
( defun setcomputertrainmarker (markerlist value)
    (print (cons (first markerlist) (cons value ())))
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

;this function helps to move a latest boneyard tile from a user pile to one of the trains
;given that atleast one of the train is playable.
( defun Playboneyardtile ( humantiles computertiles boneyardtiles humantrain computertrain mexicantrain roundnumber enginetile humannext markerslist
    tiletoadd )
    (print "Please enter the train where you want to place the playable boneyard tile>>")

    (let*
        (   (traininput (readtrain))
            (traintoplay (Gettrain traininput humantrain computertrain mexicantrain ))
            (tileplayable (checktrain traintoplay tiletoadd))
        )
        
         (cond 
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
                                        (updatedhumantrain (addTiletoEnd humantrain tiletoadd))
                                    )
                                    ;--------------------expressions below this---------------------------------------
                                    (print "Boneyard tile has been played to the human train")
                                    (playround humantiles computertiles boneyardtiles updatedhumantrain computertrain mexicantrain roundnumber enginetile humannext markerslist)
                            ))
                            ;user wants to put the boneyard to to mexican train
                            ((string-equal traininput "M")
                                (let* (
                                        (updatedmexicantrain (addTiletoEnd mexicantrain tiletoadd ))
                                    )
                                    ;--------------------expressions below this---------------------------------------
                                    (print "Boneyard tile has been played to the mexican train")
                                    (playround humantiles computertiles boneyardtiles humantrain computertrain updatedmexicantrain roundnumber enginetile humannext markerslist)
                                ))
                            ;user wants to put the boneyard tile to the computer train.
                            ;in this case checks if the computer train is marked or not.
                            ((and (string-equal traininput "C") (eq (Computertrainmarked markerslist) t))
                                (let* (
                                        (updatedcomputertrain (addTiletoEnd computertrain tiletoadd ))
                                    )   
                                    ;--------------------expressions below this---------------------------------------
                                    (print "Boneyard tile has been played to the computer train")
                                    (playround humantiles computertiles boneyardtiles humantrain updatedcomputertrain mexicantrain roundnumber enginetile humannext markerslist)
                            ))
                            ;in this case, tile is not playable on input train. So user is asked again to enter train to play.
                            (t  (print "Invalid choice!!Pick valid train between Human, Mexican and Computer train.")
                                (Playboneyardtile humantiles computertiles boneyardtiles humantrain computertrain mexicantrain roundnumber enginetile humannext markerslist
                                tiletoadd))
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
                                        (updatedcomputertrain (addTiletoEnd computertrain tiletoadd))
                                    )
                                    ;--------------------expressions below this---------------------------------------
                                    (print "Boneyard tile has been played to the computer train")
                                    (playround humantiles computertiles boneyardtiles humantrain updatedcomputertrain mexicantrain roundnumber enginetile humannext markerslist)
                            ))
                            ;computer player wants to put the boneyard to to mexican train
                            ((string-equal traininput "M")
                                (let* (
                                        (updatedmexicantrain (addTiletoEnd mexicantrain tiletoadd ))
                                    )
                                    ;--------------------expressions below this---------------------------------------
                                    (print "Boneyard tile has been played to the mexican train")
                                    (playround humantiles computertiles boneyardtiles humantrain computertrain updatedmexicantrain roundnumber enginetile humannext markerslist)
                                ))
                            ;computer player wants to put the boneyard tile to the human train.
                            ;in this case checks if the computer train is marked or not.
                            ((and (string-equal traininput "H") (eq (Humantrainmarked markerslist) t))
                                (let* (
                                        (updatedhumantrain (addTiletoEnd humantrain tiletoadd ))
                                    )   
                                    ;--------------------expressions below this---------------------------------------
                                    (print "Boneyard tile has been played to the computer train")
                                    (playround humantiles computertiles boneyardtiles updatedhumantrain computertrain mexicantrain roundnumber enginetile humannext markerslist)
                            ))
                            ;in this case, tile is not playable on input train. So user is asked again to enter train to play.
                            (t  (print "Invalid choice!!Pick valid train between Human, Mexican and Computer train.")
                                (Playboneyardtile humantiles computertiles boneyardtiles humantrain computertrain mexicantrain roundnumber enginetile humannext markerslist
                                tiletoadd))
                        )
                 
                    )
                )
            )
            ;in this case train chosen is invalid so rechoose the train.
            (t 
                (print "Invalid choice!!Pick valid train between Human, Mexican and Computer train.")
                (Playboneyardtile humantiles computertiles boneyardtiles humantrain computertrain mexicantrain roundnumber enginetile humannext markerslist
                tiletoadd)
            )
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

(defun Removeenginetile(tile list)

    (print (elt list tile))
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



(mexican-train)

;( print (getTilelocation '3 '((9 9) 3 4) '(9 9)) )


