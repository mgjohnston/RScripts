# Battle of the Dead

A coded solution to the Riddler Classic (17th May 2019), submitted 21st May 2019.

https://fivethirtyeight.com/features/how-many-soldiers-do-you-need-to-beat-the-night-king/


## Solution

Living = n^2, Dead = n

See graph and code.

## Riddler Classic

From Greg Burnham, it had to happen eventually, at long last and not a moment too soon, The Riddler meets “Game of Thrones”:

At a pivotal moment in an epic battle between the living and the dead, the Night King, head of the army of the dead, raises all the fallen (formerly) living soldiers to join his ranks. This ability obviously presents a huge military advantage, but how big an advantage exactly?

Forget the Battle of Winterfell and model our battle as follows. Each army lines up single file, facing the other army. One soldier steps forward from each line and the pair duels — half the time the living soldier wins, half the time the dead soldier wins. If the living soldier wins, he goes to the back of his army’s line, and the dead soldier is out (the living army uses dragonglass weapons, so the dead soldier is dead forever this time). If the dead soldier wins, he goes to the back of their army’s line, but this time the (formerly) living soldier joins him there. (Reanimation is instantaneous for this Night King.) The battle continues until one army is entirely eliminated.

What starting sizes of the armies, living and dead, give each army a 50-50 chance of winning?

