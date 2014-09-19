'///////////////////////////////////////////////////////////////////////////
'                               Cubix v1.0
'
'                            LokoSoft 10/12/01
'                              
'                           Programmed/Designed by:
'
'                               Casey Dunham
'
'///////////////////////////////////////////////////////////////////////////
DEFINT A-Z

CONST WELLWIDTH = 10        ' Width of playing field (well).   Range 5 to 13.
CONST WELLHEIGHT = 21       ' Height of playing field.  Range 4 to 21.
CONST NUMSTYLES = 7         ' Number of unique shapes.  Range 1 to 20.  Make sure you read the notes above the DATA statements at the end of the main program before you change this number!
CONST ROTATEDIR = 1         ' Control rotation of blocks. Can be 1 for clockwise, or 3 for counterclockwise.

CONST FALSE = 0             ' 0 means FALSE.
CONST True = NOT FALSE      ' Anything but 0 can be thought of as TRUE.
CONST SPACEBAR = 32         ' ASCII value for space character. Drops the shape.
CONST DOWNARROW = 80        ' Down arrow key.  Drops the shape.
CONST RIGHTARROW = 77       ' Right arrow key.  Moves the shape right.
CONST UPARROW = 72          ' Up arrow key.  Rotates the shape.
CONST LEFTARROW = 75        ' Left arrow key.  Moves the shape left.

CONST QUIT = "Q"            ' Q key.  Quits the game.
CONST PAUSE = "P"           ' P key.  Pauses the game.

CONST XMATRIX = 3           ' Width of the matrix that forms each falling unit.  See the discussions in Suggested Changes #2 and #3.
CONST YMATRIX = 1           ' Depth of the matrix that forms each falling unit.
CONST BYTESPERBLOCK = 76    ' Number of bytes required to store one block in Screen mode 7.
CONST BLOCKVOLUME = (XMATRIX + 1) * (YMATRIX + 1)   ' Number of blocks in each shape.
CONST ELEMENTSPERBLOCK = BLOCKVOLUME * BYTESPERBLOCK \ 2    ' Number of INTEGER array elements needed to store an image of a shape.
CONST XSIZE = 13            ' Width, in pixels, of each block.  QBlocks assumes that the entire screen is 25 blocks wide.  Since the screen is 320 pixels wide, each block is approximately 13 pixels wide.
CONST YSIZE = 8             ' Height, in pixels, of each block.  Again, QBlocks assumes that screen is 25 blocks high.  At 200 pixels down, each block is exactly 8 pixels high.
CONST XOFFSET = 4          ' X position, in blocks, of the well.
CONST YOFFSET = 2           ' Y position, in blocks, of the well.
CONST WELLX = XSIZE * XOFFSET   ' X position, in pixels, of the start of the well.
CONST WELLY = YSIZE * YOFFSET   ' Y position.

CONST WELLCOLOR7 = 0        ' Well color for SCREEN 7.
CONST BORDERCOLOR7 = 15     ' Border color for SCREEN 7.

CONST NEXTLEVEL = 300
CONST BASESCORE = 1000

TYPE BlockType              ' Block datatype.
    X AS INTEGER            ' Horizontal location within the well.
    Y AS INTEGER            ' Vertical location within the well.
    Style AS INTEGER        ' Define shape (and color, indirectly).
    Rotation AS INTEGER     ' 4 possible values (0 to 3).
END TYPE


' SUB and FUNCTION declarations
DECLARE SUB QuitGame ()
DECLARE SUB PauseGame ()
DECLARE SUB ShowTitle ()
DECLARE SUB UpdateScore ()
DECLARE FUNCTION GameOver% ()
DECLARE FUNCTION CheckFit ()
DECLARE SUB AddBlockToWell ()
DECLARE SUB CheckForFullRows ()
DECLARE SUB center (M$, Row)
DECLARE SUB DeleteChunk (Highest%, Lowest%)
DECLARE SUB DrawBlock (X, Y, FillColor)
DECLARE SUB InitScreen ()
DECLARE SUB NewBlock ()
DECLARE SUB PerformGame ()
DECLARE SUB Show (B AS BlockType)
DECLARE SUB PutBlock (B AS BlockType)
DECLARE SUB DrawAllShapes ()
DECLARE SUB DrawPlayingField ()

DIM SHARED Level AS INTEGER                                         ' Difficulty level.  0 is slowest, 9 is fastest.
DIM SHARED Score AS LONG
DIM SHARED PrevScore AS LONG

DIM SHARED WellBlocks(WELLWIDTH, WELLHEIGHT) AS INTEGER             ' 2 dimensional array to hold the falling shapes that have stopped falling and become part of the well.
DIM SHARED CurBlock AS BlockType                                    ' The falling shape.
DIM SHARED BlockShape(0 TO XMATRIX, 0 TO YMATRIX, 1 TO NUMSTYLES)   ' Holds the data required to make each shape.  Values determined by the DATA statements at the end of this window.
DIM SHARED ScreenWidth AS INTEGER                                   ' Width of the screen, in character-sized units.
DIM SHARED ScreenMode AS INTEGER                                    ' Value of the graphics screen mode used.
DIM SHARED WellColor AS INTEGER                                     ' Color inside the well.
DIM SHARED BorderColor AS INTEGER                                   ' Color of well border and text.
DIM SHARED OldBlock AS BlockType                                    ' An image of the last CurBlock.  Used to erase falling units when they move.
DIM SHARED TargetTime AS SINGLE                                     ' Time to move the shape down again.
                           
DIM SHARED Temp(11175)  AS INTEGER                                  ' Used by several GET and PUT statements to store temporary screen images.
DIM SHARED BlockColor(1 TO NUMSTYLES) AS INTEGER                    ' Block color array
DIM SHARED BlockImage((NUMSTYLES * 4 + 3) * ELEMENTSPERBLOCK) AS INTEGER    ' Holds the binary image of each rotation of each shape for the PutBlock subprogram to use.

DIM SHARED CheatEnabled   AS INTEGER


    

    RANDOMIZE TIMER                     ' Create a new sequence of random numbers based on the clock.
                         

    
    ' Read the pattern for each QBlocks shape.
    FOR i = 1 TO NUMSTYLES
        FOR j = 0 TO YMATRIX
            FOR k = 0 TO XMATRIX
                READ BlockShape(k, j, i)
            NEXT k
        NEXT j
    NEXT i
   
    DrawAllShapes                       ' Draw all shapes in all four rotations.
   
    ShowTitle
    PerformGame

    END

' Data for Style 1: Long
DATA 1,1,1,1
DATA 0,0,0,0

' Data for Style 2: L Right
DATA 1,1,1,0
DATA 0,0,1,0

' Data for Style 3: L Left
DATA 0,1,1,1
DATA 0,1,0,0

' Data for Style 4: Z Right
DATA 1,1,0,0
DATA 0,1,1,0

' Data for Style 5: Z Left
DATA 0,1,1,0
DATA 1,1,0,0

' Data for Style 6: T
DATA 1,1,1,0
DATA 0,1,0,0

' Data for Style 7: Square
DATA 0,1,1,0
DATA 0,1,1,0

'----------------------------------------------------------------------------
' AddBlockToWell
'
'    After a shape stops falling, put it into the WellBlocks array
'    so later falling shapes know where to stop.
'
'           PARAMETERS:    None.
'----------------------------------------------------------------------------
SUB AddBlockToWell
   
    FOR i = 0 TO XMATRIX                                    ' Loop through all elements in the array.
        FOR j = 0 TO YMATRIX
            IF BlockShape(i, j, CurBlock.Style) = 1 THEN    ' If there is a block in that space.
                SELECT CASE CurBlock.Rotation               ' Use the Rotation to determine how the blocks should map into the WellBlocks array.
                    CASE 0              ' No rotation.
                        WellBlocks(CurBlock.X + i, CurBlock.Y + j) = CurBlock.Style
                    CASE 1              ' Rotated 90 degrees clockwise.
                        WellBlocks(CurBlock.X - j + 2, CurBlock.Y + i - 1) = CurBlock.Style
                    CASE 2              ' Rotated 180 degrees.
                        WellBlocks(CurBlock.X - i + 3, CurBlock.Y - j + 1) = CurBlock.Style
                    CASE 3               ' Rotated 270 degrees clockwise.
                        WellBlocks(CurBlock.X + j + 1, CurBlock.Y - i + 2) = CurBlock.Style
                END SELECT
            END IF
        NEXT j
    NEXT i
END SUB

'----------------------------------------------------------------------------
' Center
'
'    Centers a string of text on a specified row.
'
'           PARAMETERS:    Text$ - Text to display on the screen.
'                          Row   - Row on the screen where the text$ is
'                                  displayed.
'----------------------------------------------------------------------------
SUB center (text$, Row)

  LOCATE Row, (40 - LEN(text$)) \ 2 + 1
  PRINT text$;

END SUB

'----------------------------------------------------------------------------
' CheckFit
'
'    Checks to see if the shape will fit into its new position.
'    Returns TRUE if it fits and FALSE if it does not fit.
'
'           PARAMETERS:    None
'
'----------------------------------------------------------------------------
FUNCTION CheckFit

    CheckFit = True                     ' Assume the shape will fit.
   
    FOR i = 0 TO XMATRIX                ' Loop through all the blocks in the
        FOR j = 0 TO YMATRIX            ' shape and see if any would
                                        ' overlap blocks already in the well.
            IF BlockShape(i, j, CurBlock.Style) = 1 THEN    ' 1 means that space, within the falling shape, is filled with a block.
                SELECT CASE CurBlock.Rotation               ' Base the check on the rotation of the shape.
                    CASE 0                         ' No rotation.
                        NewX = CurBlock.X + i
                        NewY = CurBlock.Y + j
                    CASE 1                         ' Rotated 90 degrees clockwise, or 270 degrees counterclockwise.
                        NewX = CurBlock.X - j + 2
                        NewY = CurBlock.Y + i - 1
                    CASE 2                         ' Rotated 180 degrees.
                        NewX = CurBlock.X - i + 3
                        NewY = CurBlock.Y - j + 1
                    CASE 3                         ' Rotated 270 degrees clockwise, or 90 degrees counterclockwise.
                        NewX = CurBlock.X + j + 1
                        NewY = CurBlock.Y - i + 2
                END SELECT

                ' Set CheckFit to false if the block would be out of the well.
                IF (NewX > WELLWIDTH - 1 OR NewX < 0 OR NewY > WELLHEIGHT - 1 OR NewY < 0) THEN
                    CheckFit = FALSE
                    EXIT FUNCTION

                ' Otherwise, set CheckFit to false if the block overlaps
                ' an existing block.
                ELSEIF WellBlocks(NewX, NewY) THEN
                    CheckFit = FALSE
                    EXIT FUNCTION
                END IF

            END IF
        NEXT j
    NEXT i

END FUNCTION

'----------------------------------------------------------------------------
' CheckForFullRows
'
'    Checks for filled rows.  If a row is filled, delete it and move
'    the blocks above down to fill the deleted row.
'
'           PARAMETERS:   None
'----------------------------------------------------------------------------
SUB CheckForFullRows

    DIM RowsToDelete(WELLHEIGHT)    ' Temporary array to track rows that should be deleted.
    NumRowsToDelete = 0
    i = WELLHEIGHT                  ' Begin scanning from the bottom up.
    DO
        DeleteRow = True            ' Assume the row should be deleted.
        j = 0
        DO                          ' Scan within each row for blocks.
            DeleteRow = DeleteRow * SGN(WellBlocks(j, i)) ' If any position is blank, DeleteRow is 0 (FALSE).
            j = j + 1
        LOOP WHILE DeleteRow = True AND j < WELLWIDTH
       
        IF DeleteRow = True THEN
            ' Walk up the rows and copy them down in the WellBlocks array.
            NumRowsToDelete = NumRowsToDelete + 1   ' Number of rows to delete.
            RowsToDelete(i - NumDeleted) = True     ' Mark the rows to be deleted, compensating for rows that have already been deleted below it.
            NumDeleted = NumDeleted + 1             ' Compensates for rows that have been deleted already.
           
            ' Logically delete the row by moving all WellBlocks values down.
            FOR Row = i TO 1 STEP -1
                FOR Col = 0 TO WELLWIDTH
                    WellBlocks(Col, Row) = WellBlocks(Col, Row - 1)
                NEXT Col
            NEXT Row
        ELSE                        ' This row will not be deleted.
            i = i - 1
        END IF
    LOOP WHILE i >= 1                ' Stop looping when the top of the well is reached.
           
    IF NumRowsToDelete > 0 THEN
        Score = Score + 100 * NumRowsToDelete  ' Give 100 points for every row.
       
        ' Set Highest and Lowest such that any deleted row will initially set them.
        Highest = -1
        Lowest = 100
       
        ' Find where the highest and lowest rows to delete are.
        FOR i = WELLHEIGHT TO 1 STEP -1
            IF RowsToDelete(i) = True THEN
                IF i > Highest THEN Highest = i
                IF i < Lowest THEN Lowest = i
            END IF
        NEXT i
        
        IF (Highest - Lowest) + 1 = NumRowsToDelete THEN    ' Only one contiguous group of rows to delete.
            DeleteChunk Highest, Lowest
        ELSE                                                ' Two groups of rows to delete.
            ' Begin at Lowest and scan down for a row NOT to be deleted.
            ' Then delete everything from Lowest to the row not to be deleted.
            i = Lowest
            DO WHILE i <= Highest
                IF RowsToDelete(i) = FALSE THEN
                    DeleteChunk i - 1, Lowest
                    EXIT DO
                ELSE
                    i = i + 1
                END IF
            LOOP
           
            ' Now look for the second group and delete those rows.
            Lowest = i
            DO WHILE RowsToDelete(Lowest) = FALSE
                Lowest = Lowest + 1
            LOOP
            DeleteChunk Highest, Lowest
       
        END IF
    END IF

END SUB

'----------------------------------------------------------------------------
' DeleteChunk
'
'    Deletes a group of one or more rows.
'
'           PARAMETERS:    Highest - Highest row to delete (physically lowest
'                                    on screen).
'                          Lowest  - Lowest row to delete (physically highest
'                                    on screen).
'----------------------------------------------------------------------------
SUB DeleteChunk (Highest, Lowest)
   
    ' GET the image of the row to delete.                          
    GET (WELLX, Lowest * YSIZE + WELLY)-(WELLX + WELLWIDTH * XSIZE, (Highest + 1) * YSIZE + WELLY - 1), Temp
   
    ' Flash the rows 3 times.
    FOR Flash = 1 TO 3
        PUT (WELLX, Lowest * YSIZE + WELLY), Temp, PRESET
        DelayTime! = TIMER + .02
        DO WHILE TIMER < DelayTime!: LOOP
        PUT (WELLX, Lowest * YSIZE + WELLY), Temp, PSET
        DelayTime! = TIMER + .02
        DO WHILE TIMER < DelayTime!: LOOP
    NEXT Flash
   
    ' Move all the rows above the deleted ones down.
    GET (WELLX, WELLY)-(WELLX + WELLWIDTH * XSIZE, Lowest * YSIZE + WELLY), Temp
    PUT (WELLX, (Highest - Lowest + 1) * YSIZE + WELLY), Temp, PSET
    'Erase the area above the block which just moved down.
    LINE (WELLX, WELLY)-(WELLX + WELLWIDTH * XSIZE, WELLY + (Highest - Lowest + 1) * YSIZE), WellColor, BF
END SUB

'----------------------------------------------------------------------------
' DrawAllShapes
'
'    Quickly draws all shapes in all four rotations.  Uses GET
'    to store the images so they can be PUT onto the screen
'    later very quickly.
'
'           PARAMETERS:    None.
'----------------------------------------------------------------------------
SUB DrawAllShapes

    DIM B AS BlockType
    SCREEN 7                  ' Set the appropriate screen mode.
   
    ' On EGA and VGA systems, appear to blank the screen.
   
        DIM Colors(0 TO 15)             ' DIM an array of 16 elements.  By default, all elements are 0.
        PALETTE USING Colors            ' Redefine the colors all to 0.
        FOR i = 1 TO NUMSTYLES          ' Set block colors EGA, VGA
            BlockColor(i) = ((i - 1) MOD 7) + 1
        NEXT i

    CLS
    Count = 0                           ' Count determines how many shapes have been drawn on the screen and vertically where.
    FOR shape = 1 TO NUMSTYLES          ' Loop through all shapes.

        RtSide = 4
        DO
            IF BlockShape(RtSide - 1, 0, shape) = 1 OR BlockShape(RtSide - 1, 1, shape) = 1 THEN EXIT DO
            RtSide = RtSide - 1
        LOOP UNTIL RtSide = 1

        LtSide = 0
        DO
            IF BlockShape(LtSide, 0, shape) = 1 OR BlockShape(LtSide, 1, shape) = 1 THEN EXIT DO
            LtSide = LtSide + 1
        LOOP UNTIL LtSide = 3

        FOR Rotation = 0 TO 3           ' Loop through all rotations.
            B.X = Rotation * 4 + 2      ' Determine where to put the shape.
            B.Y = Count + 2
            B.Rotation = Rotation
            B.Style = shape
            Show B                      ' Draw the shape.
           
            X = B.X: Y = B.Y
            SELECT CASE Rotation        ' Based on Rotation, determine where the shape really is on the screen.
                CASE 0                  ' No rotation.
                    x1 = X: x2 = X + RtSide: y1 = Y: y2 = Y + 2
                CASE 1                  ' Rotated 90 degrees clockwise.
                    x1 = X + 1: x2 = X + 3: y1 = Y - 1: y2 = Y + RtSide - 1
                CASE 2                  ' 180 degrees.
                    x1 = X: x2 = X + 4 - LtSide: y1 = Y: y2 = Y + 2
                CASE 3                  ' Rotated 270 degrees clockwise.
                    x1 = X + 1: x2 = X + 3: y1 = Y - 1: y2 = Y + 3 - LtSide
            END SELECT
           
            ' Store the image of the rotated shape into an array for fast recall later.
            GET (x1 * XSIZE, y1 * YSIZE)-(x2 * XSIZE, y2 * YSIZE), BlockImage(((shape - 1) * 4 + Rotation) * ELEMENTSPERBLOCK)
       
        NEXT Rotation
       
        Count = Count + 5               ' Increase Count by 5 to leave at least one blank line between shapes.
        IF Count = 20 THEN              ' No space for any more shapes.
            CLS
            Count = 0
        END IF
   
    NEXT shape
   
    CLS
   
    ' Changes the color palette if SCREEN is used.
   
        PALETTE                         ' Restore default color settings.
        PALETTE 6, 14                   ' Make brown (6) look like yellow (14).
        PALETTE 14, 15                  ' Make yellow (14) look like bright white (15).

END SUB

'----------------------------------------------------------------------------
' DrawBlock
'
'    Draws one block of a QBlocks shape.
'
'           PARAMETERS:    X         - Horizontal screen location.
'                          Y         - Vertical screen location.
'                          FillColor - The primary color of the block.
'                                      The top and left edges will be the
'                                      brighter shade of that color.
'----------------------------------------------------------------------------
SUB DrawBlock (X, Y, FillColor)

    LINE (X * XSIZE + 2, Y * YSIZE + 2)-((X + 1) * XSIZE - 2, (Y + 1) * YSIZE - 2), FillColor, BF
    LINE (X * XSIZE + 1, Y * YSIZE + 1)-((X + 1) * XSIZE - 1, Y * YSIZE + 1), FillColor + 8
    LINE (X * XSIZE + 1, Y * YSIZE + 1)-(X * XSIZE + 1, (Y + 1) * YSIZE - 1), FillColor + 8

END SUB

'----------------------------------------------------------------------------
' DrawPlayingField
'
'    Draws the playing field, including the well, the title, the
'    score/level box, etc.
'
'           PARAMETERS:   None
'----------------------------------------------------------------------------
SUB DrawPlayingField
   
        WellColor = WELLCOLOR7
        BorderColor = BORDERCOLOR7

   
    ScreenWidth = 40                      ' Set to proper width and colors.
   
    
    ' Draw the well box.
    LINE (WELLX - 1, WELLY - 5)-(WELLX + WELLWIDTH * XSIZE + 1, WELLY + WELLHEIGHT * YSIZE + 1), WellColor, BF
    LINE (WELLX - 1, WELLY - 5)-(WELLX + WELLWIDTH * XSIZE + 1, WELLY + WELLHEIGHT * YSIZE + 1), BorderColor, B
   
    ' Draw the title box.
    'LINE (XSIZE, WELLY - 5)-(XSIZE * 8, WELLY + 12), WellColor, BF
    'LINE (XSIZE, WELLY - 5)-(XSIZE * 8, WELLY + 12), BorderColor, B
   
                                         
    'COLOR 12
    'LOCATE 3, 5: PRINT "Cubix"     ' Center the program name on line 2.
    'COLOR BorderColor


END SUB

'----------------------------------------------------------------------------
' GameOver
'
'    Ends the game and asks the player if he/she wants to play
'    again.  GameOver returns TRUE if the player wishes to stop
'    or FALSE if the player wants another game.
'
'           PARAMETERS:   None
'----------------------------------------------------------------------------
FUNCTION GameOver

       LINE (55, 60)-(180, 120), 15, B
       LINE (56, 61)-(179, 119), 0, BF

       LOCATE 10, 11: PRINT "Game Over!"
       LOCATE 12, 11: PRINT "Try Again?"
       LOCATE 14, 13: PRINT "(Y/N)"

       DO
          SELECT CASE INKEY$
                CASE "y"
                        GameOver = FALSE
                        EXIT DO
                CASE "n"
                        GameOver = True
                        EXIT DO
          END SELECT
       LOOP

   
END FUNCTION

'----------------------------------------------------------------------------
' InitScreen
'
'    Draws the playing field and ask for the desired starting level.
'
'           PARAMETERS:   None
'----------------------------------------------------------------------------
SUB InitScreen


    DrawPlayingField                ' Draw playing field assuming Level 0.


    LOCATE 5, 30: PRINT "Cubix"
    LOCATE 9, 26: PRINT "Level: " + STR$(Level)
    LOCATE 11, 26: PRINT "Score: " + STR$(Score)

    LINE (198, 11)-(299, 100), 15, B
END SUB

'----------------------------------------------------------------------------
' NewBlock
'
'    Initializes a new falling shape about to enter the well.
'
'           PARAMETERS:   None
'----------------------------------------------------------------------------
SUB NewBlock

    CurBlock.Style = INT(RND(1) * NUMSTYLES) + 1    ' Randomly pick a block style.
    CurBlock.X = (WELLWIDTH \ 2) - 1                ' Put the new shape in the horizontal middle of the well
    CurBlock.Y = 0                                  ' and at the top of the well.
    CurBlock.Rotation = 0                           ' Begin with no rotation.

END SUB

SUB PauseGame
      
       GET (55, 60)-(180, 100), Temp
       LINE (55, 60)-(180, 100), 4, B
       LINE (56, 61)-(179, 99), 0, BF

       COLOR 4
       LOCATE 10, 13: PRINT "Paused"
       COLOR 15

       DO: LOOP UNTIL INKEY$ = "p"

       PUT (55, 60), Temp, PSET
END SUB

'----------------------------------------------------------------------------
' PerformGame
'
'    Continues to play the game until the player quits.
'
'           PARAMETERS:   None
'----------------------------------------------------------------------------
SUB PerformGame
    
    DO                                          ' Loop for repetitive games
        a$ = ""
        ERASE WellBlocks                        ' Set all the elements in the WellBlocks array to 0.
       
        Score = 0
        Level = 0                               ' Assume Level 0.
       
        InitScreen                              ' Prepare the screen and get the difficulty level.
       
        TargetTime = TIMER + 1 / (Level + 1)    ' TargetTime is when the falling shape will move down again.
        DO                                      ' Create new falling shapes until the game is over.
            DoneWithThisBlock = FALSE           ' This falling shape is not done falling yet.
            NewBlock                            ' Create a new falling unit.
            IF CheckFit = FALSE THEN EXIT DO    ' If it does not fit, then the game is over.
            PutBlock CurBlock                   ' Display the new shape.
           
            DO                                  ' Continue dropping the falling shape.
                OldBlock = CurBlock             ' Save current falling shape for possible later use.
                DO                              ' Loop until enough time elapses.
                   
                    ValidEvent = True           ' Assume a key was pressed.
                    ans$ = UCASE$(INKEY$)

                    IF ans$ = PAUSE OR ans$ = QUIT THEN
                        
                   
                        SELECT CASE ans$
                            CASE PAUSE
                                PauseGame
                            CASE QUIT
                                QuitGame
                        END SELECT

                    ELSE    ' A key was pressed but not Q or P.
                        ans = ASC(RIGHT$(CHR$(0) + ans$, 1))    ' Convert the key press to an ASCII code for faster processing.
                        SELECT CASE ans
                        CASE DOWNARROW, SPACEBAR   ' Drop shape immediately.
                            DO                                  ' Loop to drop the falling unit one row at a time.
                                CurBlock.Y = CurBlock.Y + 1
                            LOOP WHILE CheckFit = True          ' Keep looping while the falling unit isn't stopped.
                            CurBlock.Y = CurBlock.Y - 1         ' Went one down too far, restore to previous.
                            TargetTime = TIMER - 1              ' Ensure that the shape falls immediately.
                        CASE RIGHTARROW
                            CurBlock.X = CurBlock.X + 1         ' Move falling unit right.
                        CASE LEFTARROW
                            CurBlock.X = CurBlock.X - 1         ' Move falling unit left.
                        CASE UPARROW
                            CurBlock.Rotation = ((CurBlock.Rotation + ROTATEDIR) MOD 4)  ' Rotate falling unit.
                        CASE ELSE
                            ValidEvent = FALSE
                    END SELECT

                    IF ValidEvent = True THEN
                        IF CheckFit = True THEN         ' If the move is valid and the shape fits in the new position,
                            PutBlock OldBlock           ' erase the shape from its old position
                            PutBlock CurBlock           ' and display it in the new position.
                            OldBlock = CurBlock
                        ELSE
                            CurBlock = OldBlock         ' If it does not fit then reset CurBlock to the OldBlock.
                        END IF
                    END IF
                END IF

                LOOP UNTIL TIMER >= TargetTime       ' Keep repeating the loop until it is time to drop the shape.  This allows many horizontal movements and rotations per vertical step.
               
                TargetTime = TIMER + 1 / (Level + 1) ' The player has less time between vertical movements as the skill level increases.
                CurBlock.Y = CurBlock.Y + 1          ' Try to drop the falling unit one row.

                IF CheckFit = FALSE THEN             ' Cannot fall any more.
                    DoneWithThisBlock = True         ' Done with this block.
                    CurBlock = OldBlock
                END IF
               
                PutBlock OldBlock                    ' Erase the falling shape from the old position,
                PutBlock CurBlock                    ' and display it in the new position.
                OldBlock = CurBlock

            LOOP UNTIL DoneWithThisBlock             ' Continue getting keys and moving shapes until the falling shape stops.
           
            AddBlockToWell                           ' Shape has stopped so logically add it to the well.
            CheckForFullRows                         ' Check to see if a row(s) is now full.  If so, deletes it.
            UpdateScore

        LOOP                                         ' Unconditional loop.  Each game is stopped by the EXIT DO command at the top of this loop that executes when a new block will not fit in the well.
    LOOP UNTIL GameOver                            ' GameOver is always TRUE (-1) unless the user presses X or the well is full.

END SUB

'----------------------------------------------------------------------------
' PutBlock
'
'    Uses very fast graphics PUT command to draw the shape.
'
'           PARAMETERS:    B - Block to be put onto the screen.
'----------------------------------------------------------------------------
SUB PutBlock (B AS BlockType)
   
    SELECT CASE B.Rotation          ' Base exact placement on the rotation.
        CASE 0                      ' No rotation.
            x1 = B.X: y1 = B.Y
        CASE 1                      ' Rotated 90 degrees clockwise, or 270 degrees counterclockwise.
            x1 = B.X + 1: y1 = B.Y - 1
        CASE 2                      ' Rotated 180 degrees.
            x1 = B.X: y1 = B.Y
        CASE 3                      ' Rotated 270 degrees clockwise, or 90 degrees counterclockwise.
            x1 = B.X + 1: y1 = B.Y - 1
    END SELECT
   
    ' Actually PUT the rotated shape on the screen.  The XOR option makes the
    ' new image blend with whatever used to be there in such a way that
    ' identical colors cancel each other out.  Therefore, one PUT with the XOR
    ' option can draw an object while the second PUT to that same location
    ' erases it without affecting anything else near it.  Often used for animation.

    PUT (x1 * XSIZE + WELLX, y1 * YSIZE + WELLY), BlockImage(((B.Style - 1) * 4 + B.Rotation) * ELEMENTSPERBLOCK), XOR  ' XOR mixes what used to be there on the screen with the new image.  Two identical colors cancel each other.

END SUB

SUB QuitGame
      
       GET (55, 60)-(180, 120), Temp

       LINE (55, 60)-(180, 120), 15, B
       LINE (56, 61)-(179, 119), 0, BF

       LOCATE 10, 13: PRINT "Quit"
       LOCATE 12, 9: PRINT "Are You Sure?"
       LOCATE 14, 13: PRINT "(Y/N)"

       DO
          SELECT CASE INKEY$
                CASE "y"
                        END
                CASE "n"
                        
                        EXIT DO
          END SELECT
       LOOP

       PUT (55, 60), Temp, PSET
END SUB

'----------------------------------------------------------------------------
' Show
'
'    Draws the falling shape one block at a time.  Only used by
'    DisplayAllShapes.  After that, PutBlock draws all falling
'    shapes.
'
'           PARAMETERS:    B - Block to be put onto the screen.
'----------------------------------------------------------------------------
SUB Show (B AS BlockType)
                                                 
    ' Loop through all possible block locations.
    FOR i = 0 TO XMATRIX
        FOR j = 0 TO YMATRIX
           
            IF BlockShape(i, j, B.Style) = 1 THEN   ' 1 means there is a block there.
                 SELECT CASE B.Rotation             ' Exact screen position is determined by the rotation.
                    CASE 0                          ' No rotation.
                        DrawBlock B.X + i, B.Y + j, BlockColor(B.Style)
                    CASE 1                          ' Rotated 90 degrees clockwise, or 270 degrees counterclockwise.
                        DrawBlock B.X - j + 2, B.Y - 1 + i, BlockColor(B.Style)
                    CASE 2                          ' Rotated 180 degrees.
                        DrawBlock B.X + 3 - i, B.Y - j + 1, BlockColor(B.Style)
                    CASE 3                          ' Rotated 270 degrees clockwise, or 90 degrees counterclockwise.
                        DrawBlock B.X + j + 1, B.Y - i + 2, BlockColor(B.Style)
                END SELECT
            END IF
        NEXT j
    NEXT i

END SUB

SUB ShowTitle

        CLS
        LINE (0, 0)-(320, 200), 15, B
        LINE (1, 1)-(319, 199), 7, B

        center "Cubix", 5
        center "v 1.0", 8
        center "Brought to you by:", 12
        center "LokoSoft", 14
        center "Programmed by:", 16
        center "Casey Dunham", 18
        center "<Press any key to begin>", 20

        DO
        LOOP UNTIL INKEY$ <> ""

        CLS
        
END SUB

SUB UpdateScore

        IF Level < 9 AND Score >= (NEXTLEVEL * (Level + 1) + PrevScore) THEN
                GET (WELLX, WELLY)-(WELLX + WELLWIDTH * XSIZE, WELLY + WELLHEIGHT * YSIZE), Temp

                PrevScore = Score
                Level = Level + 1
                DrawPlayingField
                PUT (WELLX, WELLY), Temp
        END IF

    LOCATE 9, 26: PRINT "Level: " + STR$(Level)
    LOCATE 11, 26: PRINT "Score: " + STR$(Score)

END SUB

