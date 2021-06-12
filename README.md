# Snake-Game-DOSBOX
A snake game developed purely using assembly language in DOSBOX. 

## Execution
First of all in order to run the application
1. Install `DOSBOX`
2. Download `nasm` and extract the file in the directory which contains the above .asm file.You can do that with this [Link](https://theiteducation.com/how-to-install-nasm-on-windows-10-how-to-type-and-run-assembly-language-program/). It gives you all the step wise details to download nasm and how to mount the drive that contains nasm and DOSBox.
3. Mount the directory which contains nasm and .asm file in DOSBOX.
4. Run the following command:


`nasm snake.asm -o snake.com`


After this command, run this command and the game will start:
`snake.com`
Once you have created this snake.com file, you may directly run the command after you have mount to the drive of the project and navigate where file is:


`snake.com` 

and play the game.

## Features of the game:
1. The following functionalities have been implemented:
1. Initially snake moves in the right direction automatically.
2. You can move the snake in different directions using keyboard up, down, left and right keys.
3. The snake's initial size is 20 characters, with the head -( represented by `0`) of the snake being represented by a different character than the body.
4. The fruits are displayed over the screen at a random position and stays there until they are eaten by the snake. If the snake eats the fruit, the `points` are increased by 4,  the size of the snake is also increased by 4 characters and next fruit immediately appear on the screen randomly.
5. Dangerous fruits are also displayed on the screen and they blink to indicate they are dangerous.
6. The maximum size of the snake can be 240 characters and snake needs to reach that in 4 minutes in order for the player to earn some points and advance to next level. Timer is displayed above. Moreover when the player reaches certain points, they also are updated to next level.
7. Initially, the player has three lives. He loses life if:
* It's head collides with/touches the border of the screen
* Eat's a `dangerous fruit` 
* If the snake does not reach 240 characters size in 4 minutes.
* If the snake touches itself


After every 20 seconds the speed of snake is twice as the previous speed.


There are 3 levels in the above developed game. Each level is difficult and different barriers are added in each level.

### Level 1
![image](https://user-images.githubusercontent.com/68595241/121787749-7242e980-cbe1-11eb-9c90-ef67f876871b.png)
### Level 2
Oops! I lost! :)


![image](https://user-images.githubusercontent.com/68595241/121786655-b67ebb80-cbda-11eb-8358-20b4361c27af.png)
### Level 3
Again! Lost the game! I hope you're a better player than I am! :)


![image](https://user-images.githubusercontent.com/68595241/121787789-acac8680-cbe1-11eb-9741-8e447fac7b51.png)

Enjoy the game!




