import Player
from Names.Name_Gen import random_name
from Player import p_class

new_player = Player.p_class.new_player

# Game greeting
print("\nWelcome to a classic choose your own adventure game!")
print("You are to follow along with the story and choose a branching path that you desire.\n")

# Character Creation while loop
print("If you wish to create a custom character enter 'custom'. If you wish to have a random character made for you "
      "enter 'random'.")
print("NOTE: Your age, weight, and gender will have an effect on the game.")

char_choice = input("Enter your choice: ")
while char_choice != "random" and char_choice != "Random" and char_choice != "custom" and char_choice != "Custom":
    print("You entered invalid choice. Please choose between 'custom' and 'random'. "
          "Be sure to check your spelling!\n")
    char_choice = input("Enter your choice: ")

# Creates Random Character from Player Class
if char_choice == "random" or char_choice == "Random":
    new_player = Player.p_class.create_player_random()


# Allows User to create their own character details
elif char_choice == "custom" or char_choice == "Custom":
    print("\nCharacter Name")
    np_name = input("Enter your character's name: ")

    np_age = int(input("Enter your character's age: "))
    while int(np_age) <= 18 or int(np_age) >= 100:
        print("\nAge must be between 18 and 100. Try again bucko.")
        np_age = int(input("Enter your character's age: "))

    np_weight = int(input("Enter your character's weight: "))
    while np_weight <= 95 or np_weight >= 350:
        print("\nWeight must be between 95 and 350. Try again pancake.")
        np_weight = int(input("Enter your character's weight: "))

    np_sex = input("Enter your character's gender: ")
    while np_sex != "male" and np_sex != "Male" and np_sex != "female" and np_sex != "Female":
        print("\nInvalid choice. Please try again!")
        np_sex = input("Enter your character's gender: ")
    if np_sex == "male" or np_sex == "Male":
        np_sex = "Male"
    else:
        np_sex = "Female"

    new_player = Player.p_class.create_player(np_name, np_age, np_weight, np_sex)

print("\nHere is your character's information details:")
print(new_player.player_stats())
print("If at anytime you want to see your current stats, please just enter 'player' into text box"
      "when asked to make a choice.")




# Game Starts
land_name = random_name()
village_name = random_name()
begger_name = random_name()

# Player mission/task
print("\n\nYou are", new_player.name, "a", new_player.age, "year old mercenary hired by the King of", land_name,
      "to go and investigate reports ")
print("of strange occurrences happening in a neighboring village,", village_name, ".Your task is to check on this "
                                                                                  "village and find ")
print("out what the cause of these strange occurrences are and to help by any means you can. The king has promised to ")
print("reward you with the title of lord and to live amongst the noble in luxury if you succeed.")

# Player begins
print("\nYou begin your journey with", new_player.health, "health left and", new_player.gold,
      "gold to your name as well as your trusty sword.")

print("\nIt is mid day. The sun is hot. You sweat drips down your face, stinging your eyes. You are packed up and ready.")
print("The village is a 3 days walk away, 1 day by horse. The King has offered you a horse to ease your travel.")
print("However the King also warns you of increased bandit activity along the highway. He offers you a choice,")

print('\n"I can spare you a horse for travel but I recommend you take the path through the forest. You will be less ')
print('likely to encounter any bandits there. But that also means your journey is guaranteed to be rough. I will ')
print('leave the decision to you".')

print("\nYou weigh your options carefully as quite literally you life is on the line. The outerlands is ruled by no ")
print("one other than local bandits and wildlife. This journey will not be easy, that is why the king as promised to ")
print("reward you so well. After some time, you give the king your decision.")


# First choice
print("\nEnter your choice by typing the matching number.\n")
print("Do not take the horse and go through the woods instead: (1)")
print("Take the horse and travel by the main road: (2)")
choice = 0
while choice != str(1) and choice != str(2) and choice != "player" and choice != "Player":
    choice = input("\nWhat do you do?: ")
    if choice == "player" or choice == "Player":
        print(new_player.player_stats())
        choice = str(0)

    # Choice (1)
    elif choice == str(1):

        print('\n"Ahh! A wise decision." the king shouts. "Off you go then, you must not waste any time. Your journey will ')
        print('be long so keep your wits about you. I want to see you alive again." ')

        print("\nYou thank the king and leave him and the horse behind. You make your way towards the main gate of the ")
        print("kingdom. Along the way you spot an old blind begger in the street covered in fecal matter. He is asking for ")
        print("coin to anyone he can hear pass by.")

        print("\nEnter your choice by typing the matching number.")

        print("\nDo you avoid the begger?: (1)")
        print("Do you give him some coin?: (2)")

        choice = 0
        while choice != str(1) and choice != str(2) and choice != "player" and choice != "Player":
            choice = input("\nWhat do you do?: ")
            if choice == "player" or choice == "Player":
                print(new_player.player_stats())
                choice = str(0)

            # Choice (1)
            elif choice == str(1):
                old_beg = False
                print("\nAvoiding the begger you quickly step to the side hiding behind a large passing cart. ")
                print("You don't have time for that, you're on a mission.")

            # Choice (2)
            elif choice == str(2):
                old_beg = True
                print("\nFeeling sorry and seeing as you have some extra coin you to give the begger 10 gold pieces.")
                print("(-10 Gold Pieces)")
                new_player.gold = new_player.gold - 10

                print('\n"Oh thank you!!" says the old man. "Everyone here is a good for nothing asshole who only ')
                print('care for themselves... Anyways, my name is', begger_name, ',I lived here all my life. ')
                print('Where are you off to stranger?"')

                print("\nYou tell the begger that you are on a mission from the king. Although, you cannot disclose ")
                print("any information on why you are going there. You wish the begger a good day and set off.")
            else:
                print("Invalid option")

        # Journey to woods begin
        # if choice == str(1):
        #     old_beg = False
        # else: old_beg = True
        print("\nYou continue onward towards the woods. You pass the main gate and take one last look back. You know ")
        print("your mission is dangerous and you are risking your life. You take a moment to think about your life ")
        print("within the Kingdom of", land_name, ". You take a deep breath and start your journey through the woods.")

        print("\nIt's been hours since you left the safety of the Kingdom and it's walls. Most of the woods is ")
        print("untamed landscape besides for small animal paths. The sun is starting to set and you think to yourself ")
        print("that you should setup a camp for the night. You spot cave you could sleep in, however its getting dark ")
        print("and you do not know if the cave is empty. You could sleep out in an open meadow but that leaves you ")
        print("unprotected.")

        print("\nEnter your choice by typing the matching number.")

        print("\nDo you try your luck in the cave? (1)")
        print("Do you try the open meadow? (2)")
        choice = 0
        while choice != str(1) and choice != str(2) and choice != "player" and choice != "Player":
            choice = input("\nWhat do you do?: ")

            if choice == "player" or choice == "Player":
                print(new_player.player_stats())
                choice = str(0)

            elif choice == str(1):
                print("\nYou cautiously enter the cave with you sword at the ready. You stop and listen for any "
                      "strange ")
                print("noises... You hear nothing. It's dark so you run your hand along the wall to feel the cave out. ")
                print("By doing so you note that the cave is shallow and only has a central open room. Perfect place to ")
                print("set up camp for the night. ")

                print("\nYou begin setting up a fire right in the middle of the room. You strike the pile of wood with ")
                print("your flint and a large flame engulfs your wooden pile. Now with the cave lit up, you can see for ")
                print("the first time. There is nothing special of this cave just cool and damp with moss hanging from the ")
                print("celling. As you look around you notice a hole in the back wall, you walk up and inspect it.")

                print("\nThe hole is no bigger than a door for a dog. You look inside and to your surprise you see ")
                print('something shiny in the far back of the hole. "Gold perhaps? Lost artifact?" you think to yourself.')
                print("You've heard stories of people getting trapped in caves when explored, however you can't seem ")
                print('too shake the idea of something being back there from your head. "Nobody around for hours ')
                print('why would there be something back there?" you think to yourself. You take a second to think.')

                print("\nEnter your choice by typing the matching number.")

                print("\nTry crawling through the hole? (1)")
                print("Stay within the central cave room? (2)")

                choice = 0
                while choice != str(1) and choice != str(2) and choice != "player" and choice != "Player":
                    choice = input("\nWhat do you do?: ")

                    if choice == "player" or choice == "Player":
                        print(new_player.player_stats())
                        choice = str(0)

                    elif choice == str(1):
                        if new_player.weight < 200:
                            print("\nYour curiosity gets the better of you and you crawl into the hole.")
                            print("It's a tight fit but you are slowly making your way towards the shiny object.")
                            print("However, you soon realize you cannot turn around, you are trapped in the crawlspace.")
                            print("Unable to move you die a slow lonely death.")

                            print("\nCause of death: Starvation")
                            quit()
                        else: print("\nYou are too fat to squeeze through. Maybe you should lay off the beer.")
                        print("But maybe this was a good thing as you quickly come to the realization that if you became ")
                        print("trapped. No one would be there to help you.")

                    elif choice == str(2):
                        print("\nYou decide to stay in the central room. There is no one nearby to help you if you get stuck.")

                    else:
                        print("Invalid option")

                print("\nYou lay down by the fire thinking of the rest of the journey ahead of you. ")
                print("You start slowly drifting into sleep when suddenly you hear rustling outside the cave. ")
                print("You get up as fast as you can but your heart drops when you see a Gorilla-Bear walk in. ")
                print("The dank stench off wet fur hits your nose, its so strong it can knock some people out. ")
                print("You read about these creatures before but never seen one as they are solitary creatures. ")
                print("This must be it's lair and you're squatting in it. ")

                print("\nThe bear lets out a loud roar/screech that makes your ears bleed. You're cornered and cannot ")
                print("escape. You quickly grab your sword and stand in a defensive position. But you're not stupid. ")
                print("Gorilla-Bears are some of the most strongest creatures in the wild, you have no hope of winning. ")
                print("However, you don't have much options.")

                print("\nEnter your choice by entering the matching number")

                print("\nStand your ground and fight. (1)")
                print("Improvise an escape. (2)")
                choice = 0
                while choice != str(1) and choice != str(2) and choice != "player" and choice != "Player":
                    choice = input("\nWhat do you do?: ")
                    if choice == "player" or choice == "Player":
                        print(new_player.player_stats())
                        choice = str(0)

                    elif choice == str(1):
                        #Player damage
                        print("\nYou prepare yourself for the invitable hard blow to your body. The Gorilla-Bear stands ")
                        print("On it's hind legs and raises both of it's arms above it's head, ready to slam down on you. ")
                        print("By doing so the bear left itself open; quickly thinking, you jab your sword through the  ")
                        print("heart of the beast. The beast howls is pain as it instantaneously falls to the ground. ")
                        print("by doing so, however, it crushes your leg with it's massive body, hurting you.")

                        print("\nYou've taken 30 points of damage towards your total health")
                        new_player.health = new_player.health - 30

                        print("\nAfter mending your leg a wave of exhaustion hits you. You decide to stay in the cave ")
                        print("with the dead beast. You know you'll be safe now as Gorilla-Bears are solitary creatures ")
                        print("as well as being very territorial. You quickly fall into a heavy sleep with no issues.")


                    elif choice == str(2):
                        # throw sand and slip out if young
                        # get hurt if older
                        print("\nUsing your quick thinking, you grab a handful of sand and throw it in the beast's eyes. ")
                        print("For some reason the words 'pocket sand' pop into your head... you do not know why. ")
                        print("The bear is momentarily stunned by the sand, you use this opportunity to your advantage. ")
                        print("With it's massive body blocking the entrance, the only way out seems to be between it's ")
                        print("legs. You try to make a break for it.")

                        if new_player.age < 60:
                            print("\nWith the power of your youth, you manage to roll through the beast's legs and run ")
                            print("for safety. ")

                        else:
                            new_player.health = new_player.health - 50
                            if new_player.health > 0:
                                print("\nWith your body not as young as it was, your acrobatic skill was lacking causing ")
                                print("you to fumble between it's legs. The beast starts to thrash around as a result. ")

                                print("\nYou take 50 damage for your mistake")

                                print("\nHowever, you still manage to escape and run away.")

                            else: print("\nYou died from being too old in a young person's world"), quit()

                        print("\nAfter narrowly escaping with your life you find a hidden spot between some trees. Exhausted ")
                        print("you quickly fall asleep. Your dreams plagued by what just happened, you have a restless night.")

                    else:
                        print("Invalid option")


            elif choice == str(2):
                #Start seeing figures on edge of tree line
                print("\nYou decide to find a spot to sleep in the tree line. Although the cave would be nice to sleep in ")
                print("you don't know what could be in there. It's better to be safe than sorry. At the same time however, ")
                print("you do not know if you'll be safe where you are. It's best to get a fire going.")

                print("\nYou find a section of trees that you can rest against. You start to settle down and get the fire ")
                print("set up. You catch something in the corner of your eye. You quickly look over but whatever was there ")
                print("is gone. You brush it off. You set up the fire and lean against the tree. Day one of your quest ")
                print("is almost over. All the walking you did made you sleepy, you being to doze off.")

                print("\nYou suddenly wake up to sharp pain in your leg, you scream more in shock than pain. ")
                print("You roll you pant leg up to see thousands of Piranha-Ants covering your leg. Dipping blood and ")
                print("pain beginning to swell, you realize the tree you were sleeping on is home to a colony of ")
                print("Piranha-Ants. You freak out and try to figure a way to get them off.")

                print("\nEnter your choice by typing the matching number.")

                print("\nTry and shake the ants off. (1)")
                print("Stick your leg in your campfire to burn them off. (2)")
                choice = 0
                while choice != str(1) and choice != str(2) and choice != "player" and choice != "Player":
                    choice = input("\nWhat do you do?: ")
                    if choice == "player" or choice == "Player":
                        print(new_player.player_stats())
                        choice = str(0)

                    elif choice == str(1):
                        print("\nYou try and shake them off your leg. Slapping and wiping at your leg some begin to fall off. ")
                        print("You keep brushing but now the ants are on your hand and biting.")

                        print("\nYou take 15 points of damage.")
                        new_player.health = new_player.health - 15
                        if new_player.health < 0:
                            print("\nThe ants were too much for you as they start to cover more parts of your body until ")
                            print("you're completely covered.")
                            print("\nCause of death: Eaten alive fro Piranha-Ants.")
                            quit()

                        print("\nYou keep slapping but only a few come off.")

                        print("\nKeep brushing them off. (1)")
                        print("Stick your leg in the fire. (2)")
                        choice = 0
                        while choice != str(1) and choice != str(2) and choice != "player" and choice != "Player":
                            choice = input("\nWhat do you do?: ")
                            if choice == "player" or choice == "Player":
                                print(new_player.player_stats())
                                choice = str(0)

                            elif choice == str(1):
                                print("\nYou keep trying to brush them off but at the same time they keep biting your leg. ")

                                print("\nYou take 25 more points of damage")
                                new_player.health = new_player.health - 25
                                if new_player.health < 0:
                                    print("\nThe ants were too much for you as they start to cover more parts of your body until ")
                                    print("you're completely covered.")
                                    print("\nCause of death: Eaten alive from Piranha-Ants.")
                                    quit()

                                print("\nThe ants are starting to disperse but you don't know if you can get the rest.")

                                print("\nKeep trying to wipe them off. (1)")
                                print("Stick your leg in the fire. (2)")
                                choice = 0
                                while choice != str(1) and choice != str(
                                        2) and choice != "player" and choice != "Player":
                                    choice = input("\nWhat do you do?: ")
                                    if choice == "player" or choice == "Player":
                                        print(new_player.player_stats())
                                        choice = str(0)

                                    elif choice == str(1):
                                        print("\nYou scream in pain and frustration and with all your might you go into ")
                                        print("a wiping frenzy. You attack your leg with multiple strikes and wipes. ")
                                        print("In your fury you manage to remove all the ants, however, your leg is now ")
                                        print("severely injured from your attacks.")

                                        print("\nYou take 30 points of damage.")
                                        new_player.health = new_player.health - 30
                                        if new_player.health < 0:
                                            print("\nAfter being eaten from the ants, the damaged from you cause massive blood loss.")
                                            print("You lay down looking at the night sky as the starts start to flicker out.")
                                            print("\nCause of death: Being too dumb.")
                                            quit()

                                    elif choice == str(2):
                                        print(
                                            "\nYou quickly stick your leg into the fire. You keep it in for a couple seconds until ")
                                        print("you cannot take the pain anymore.")

                                        print("\nYou take 50 points of damage.")
                                        new_player.health = new_player.health - 50
                                        if new_player.health > 0:
                                            print("\nWith your leg seared and in pain, you are relieved that the ants were burned away as ")
                                            print("well. Exhausted, you move to the other side of your fire and quickly fall asleep.")

                                        else:
                                            print("\nAlready damaged from earlier the fire was just too much for you and "
                                                  "succumb to your wounds. ")
                                            print("Cause of death: Self Inflicted Immolation.")
                                            quit()

                                    else:
                                        print("Invalid option")

                            elif choice == str(2):
                                print("\nYou quickly stick your leg into the fire. You keep it in for a couple seconds until ")
                                print("you cannot take the pain anymore.")

                                print("\nYou take 50 points of damage.")
                                new_player.health = new_player.health - 50
                                if new_player.health > 0:
                                    print("\nWith your leg seared and in pain, you are relieved that the ants were burned away as ")
                                    print("well. Exhausted, you move to the other side of your fire and quickly fall asleep.")

                                else:
                                    print("\nAlready damaged from earlier the fire was just too much for you and succumb to your wounds. ")
                                    print("Cause of death: Self Inflicted Immolation.")
                                    quit()
                            else:
                                print("Invalid option")

                    elif choice == str(2):
                        print("\nYou quickly stick your leg into the fire. You keep it in for a couple seconds until ")
                        print("you cannot take the pain anymore.")

                        print("\nYou take 50 points of damage.")
                        new_player.health = new_player.health - 50
                        if new_player.health > 0:
                            print("\nWith your leg seared and in pain, you are relieved that the ants were burned away as ")
                            print("well. Exhausted, you move to the other side of your fire and quickly fall asleep.")

                        else:
                            print("\nAlready damaged from earlier the fire was just too much for you and succumb to your wounds. ")
                            print("Cause of death: Self Inflicted Immolation.")
                            quit()

                    else:
                        print("Invalid option")


            else:
                print("Invalid option")

        #next day after sleeping
        print("\nYou awake the next morning. Still tired from the night before. You check yourself over and pack your ")
        print("things. You are on day two of your journey and are about halfway to the village of", village_name)
        print("You need to get a move on as you think to yourself that if you had taken the horses, you'd be there by ")
        print("now. After everything was packed you began your track towards", village_name)

        print("\nAfter walking through the dense woods for a couple hours you come across a strange path leading into ")
        print("an even denser part of the woods. You get a uneasy feeling when you look over there. You hear a quite ")
        print("voice telling you to look over here again. You are certain theres no one around, but you clearly heard someone talking.")
        print("Your curiosity starts to grow as you think what could be over there. What do you do?")

        print("\nListen to the voice and look back over. (1)")
        print("Continue forward without looking. (2)")
        choice = 0
        while choice != str(1) and choice != str(2) and choice != "player" and choice != "Player":
            choice = input("\nWhat do you do?: ")
            if choice == "player" or choice == "Player":
                print(new_player.player_stats())
                choice = str(0)

            elif choice == str(1):
                print("\nYou look back over at the path and it seems... different. The path itself seems to be moving ")
                print("as if it were made of water. You rub your eyes to clear your vision, when you look back the movement ")
                print("stopped. Feeling concerned you try to go back to the other path but when you turn around you ")
                print("see the same path you were just looking at. You quickly look behind you again and you see the same ")
                print("path again. Your heart sinks as you realize you are trapped, your sense of fright becomes stronger.")

                print("\nYou hear a disturbing voice coming from the end of the path. Terror is slowly sinking into your ")
                print("bones. You need to escape but the only way you see is down the path, but surely its almost ")
                print("certain death if you go. You cannot stay in the same place forever, you need to make a choice.")

                print("\nPlease enter the matching choice.")

                print("\nPress forward down the path. (1)")
                print("Do something drastic. (2)")
                choice = 0
                while choice != str(1) and choice != str(2) and choice != "player" and choice != "Player":
                    choice = input("\nWhat do you do?: ")
                    if choice == "player" or choice == "Player":
                        print(new_player.player_stats())
                        choice = str(0)

                    elif choice == str(1):
                        if old_beg == True:
                            print("\nWith the only way out being down the path you swallow your fear and press forward. ")
                            print("As you walk down the path you start to feel heavy and begin to move slow. The edge of ")
                            print("the forest starts to turn pitch black. You feel something rushing towards you. ")
                            print("An overwhelming evil presence washes over you, whatever it is you need to get away. ")

                            print("\nBut with your body being so heavy you can't begin to move, you are at the mercy of this ")
                            print("demon. You feel it's presence getting closer and closer. \n\n'You are going to die!' ")
                            print("'You are going to die!' \n'You are going to die!' \n'You are going to die!' \n'You are going to die!' ")

                            print("\nJust as you were ready to give up, you hear a familiar voice. 'Hey Ho!' the voice says. ")
                            print("You look up to see it's", begger_name, "the blind begger from the city! 'I've been watching ")
                            print("you on your journey' he says. 'I was afraid something like this might happen, I may not ")
                            print("be able to see with my eyes, but there are more than one way of seeing'. The old begger ")
                            print("begins to chant, at the same time the darkness starts reseeding and you start to feel ")
                            print("lighter. A bright light emits where the old begger was as it becomes the only thing ")
                            print("you can see. Everything fades to white and in a instant you are back on the original ")
                            print("path. You no longer see the side path and the begger is nowhere to be found. ")
                            print("You say a small prayer for him and thank him for saving your life before you set off again.")


                        elif old_beg == False:
                            print("\nWith the only way out being down the path you swallow your fear and press forward. ")
                            print("As you walk down the path you start to feel heavy and begin to move slow. The edge of ")
                            print("the forest starts to turn pitch black. You feel something rushing towards you. ")
                            print("An overwhelming evil presence washes over you, whatever it is you need to get away. ")

                            print("\nYou struggle and struggle but to no avail, you cannot move. The evil presence gets ")
                            print("closer and closer until you feel it in your ver̵̰͕̩̖͕̹̺͂̽̿̑́̚̕͘h̸̢̛̪̠͚͓̳̤͖͉̪̐͗̀́͌̈́̚ͅͅē̶̢̬̦͇͗̿̍ ̷̪͍̱̻̇̏́͊̀͋̉̅̀͘͝")
                            print("m̵̯͈̜̠̬̰͍̅͂̆̓ͅo̵\n̩̺͔̜̰̮̅̇̅̒́̐͆͂͋͒̆͋͝t̴̡̨͕͎͚͕̤̙̼̜̥͓͖̬̐͛͂͋̂̇̃̉̈́̽h̶̜͒̓̈́͑̈́̏͘͝ȩ̴͇̀̔̈̑͂̉̾̈́̾̎͐̊̕͠\nr̴̡̟͔̬̗̩̩̟͈̘͗̎̽̀͗̂̾̚͜ ̷̤̤̮̖̮̂ö̴̧̧̨̡̜̤̯̖̺͉̭̱̻̣̺́͊̅͊̂̈́f̶̲̞̠̠͕͇͔̖̭̜̿")
                            print(" ̷̢̹̞͎̰̪̜͕͇̮̯̉͂̿͐͂̀͜ͅd̶͚͉͚͉̤͙̠̹̝̰͖̞̆̂̆̔̍͜͝ͅe̴̛̩͙͓̬̬̅̇̎̋́̀̽̐͌̔͠ͅa̸̛͉̝̗̲͕̥̦̾̍̑̉̏̍̅̌̅͊͠\nt̶̟͈̥̑͐͗͝͠ḩ̵͇̫͇͖̱̍̂̈́̓̊̀̑͌͗͆̚̕̚͠ ̷͚̱͍͔̅̎̿̋̈́͝ͅì̵̟͈̖̝̈̐͐͂͌͠s̶̯̩̟̺̰͙͖͚̘̖̠͚̞͌̂͒̀͑̿̕ͅ")
                            print(" ̸͈̟͇͈̂̀̅͂̔̽͆̈́͠͠ů̶̡̔̂̅p̶̘͖͍͆͐͌̋͝ơ̷̜̮̤͋̑͗̋̈́̕̚ṉ̵̰͍͇͎͙̰͖͖̇́͋̈͒̓̒͐͌͗ ̶̛͔̞̬͕̭̰̱̗͔̫͈̜͔͖̄̐͌̃̿̕͜͝y\ņ̸̛̺̭̘̲̹̒̔̋̽͆͒̅̚͘̕̚͠͝ͅo̵̡̨̻̗̩̗̪͚̯͋͊͛̽̇̈́̀u̸̯̾")

                            print("\nYou are lost to the dark corners of the void.")
                            quit()

                    elif choice == str(2):
                        print("\nQuickly thinking and with no other options. You draw your sword out and slice your ")
                        print("hand open. You take 20 points of damage.")
                        new_player.health = new_player.health - 20
                        if new_player.health > 0:
                            print("\nThe sudden shock of pain snaps you out of whatever trance you were in. Not wanting ")
                            print("it again, you quickly run away down the original path. With only a minor injure you ")
                            print("shudder to think about what could of happened if you hadn't cut your hand. You ")
                            print("run a short distance away before taking a moment to catch your breath.")

                        else:
                            print("\nAlas this was a mistake as you were previously injured before. With your wounds ")
                            print("too great you bleed out on the spot. The fear of dying is subsided with relief ")
                            print("knowing that whatever was over there would be worse than dying.")

                            print("You succumbed to your wounds and bleed out in a forgotten area.")
                            quit()
                    else:
                        print("Invalid option")

            elif choice == str(2):
                print("\nFearing for your safety and with rational thinking you try with all your might to not look ")
                print("over a second time. You walk for an hour without looking back as precaution. You take a short ")
                print("break before continuing on your journey.")

            else:
                print("Invalid option")

        print("\nAfter some taking some time to rest you begin the rest your journey. According to your map you should ")
        print("be arriving by nightfall. With it being the evening, your are close to", village_name, "then your real ")
        print("job can begin. You continue walking down the path, the path starts to merge into a the main road! You ")
        print("are out of the forest, almost there.")





    elif choice == str(2):
        print('\n"A risky choice but I pray for your return and safe passage." the King says. "May you be swift and and ')
        print('your sword strike true." You put your things on the horse and set off on your journey.')

        print("\nIts now been two hours since you left the kingdom and are moving at a good speed. You haven't seen ")
        print("unusual but now that your are far away from the kingdom, you expect to seem something soon. Like a ")
        print("sick ironic joke, you spot something laying in the grass by the side of the road. You bring your horse's ")
        print("speed down to inspect what it is. On a closer inspection you notice it's a dead body. Dressed in a ")
        print("travelers attire you don't see anything else around.")

        print("\nPlease enter the matching number.")

        print("\nInspect the body. (1)")
        print("Ignore it. (2)")
        choice = 0
        while choice != str(1) and choice != str(2) and choice != "player" and choice != "Player":
            choice = input("\nWhat do you do?: ")
            if choice == "player" or choice == "Player":
                print(new_player.player_stats())
                choice = str(0)

            elif choice == str(1):
                print("\nYou decide to get a closer inspection of the body. You get off your horse and on your knees and ")
                print("begin to look over the body. You start checking the pockets for anything that could identify the ")
                print("body. You find nothing.\nYou hear a rustling noise behind you.")

                print("\nYou look behind you to see someone sneaking up to your horse! Just as you were about to get up ")
                print("something grabs your arm and cuts it. It was the dead body! It wasn't actually dead, this was an ambush!")

                print("\nYou lose 30 points of health.")

                print("\nYou recoil from the pain and jerk your arm away. The once dead body is now wielding a dagger ")
                print("and has it aimed at your heart.")

                print("\nPlease enter the matching number.")

                print("\nTry and roll away. (1)")
                print("Try and kill the man. (2)")
                choice = 0
                while choice != str(1) and choice != str(2) and choice != "player" and choice != "Player":
                    choice = input("\nWhat do you do?: ")
                    if choice == "player" or choice == "Player":
                        print(new_player.player_stats())
                        choice = str(0)

                    elif choice == str(1):
                        print("\nYou roll away quickly and onto your feet. The man still on the ground, you look over ")
                        print("to your horse and see the other bandit approaching it. You run over fast ")
                        print("leaving the first bandit on the ground. The bandit near your horse tries to go through ")
                        print("your saddlebags, but with the a loud whistle from you, you make the horse run off. ")
                        print("Charging at full speed with your sword, the bandit is caught off guard when the horse ")
                        print("suddenly left and has no time to defend himself. You impale the bandit with he entire ")
                        print("length of your sword.")

                        print("\nYou turn back to the other bandit now and notice he is wielding a bow and arrow! ")
                        print("He must of been hiding it in the grass. You do not have any ranged weapons at all. You ")
                        print("have to get within your sword's range, but the only way to do that is to close the distance.")
                        print("With no time to think you grab your sword and charge the bandit. He readies his bow and ")
                        print("and aims.")

                        print("\nDodge left. (1)")
                        print("Dodge right. (2)")
                        choice = 0
                        while choice != str(1) and choice != str(2) and choice != "player" and choice != "Player":
                            choice = input("\nWhat do you do?: ")
                            if choice == "player" or choice == "Player":
                                print(new_player.player_stats())
                                choice = str(0)
                            elif choice == str(1):
                                print("\nYou dodge left as the arrow wiz's past you on your right. A lucky guess but ")
                                print("you are still not within, he still has time to fire one more arrow before you ")
                                print("get to him. He once again raises and aims his bow. However, you become unsure ")
                                print("which way to dodge. Will he anticipate you going to the left again? Or will ")
                                print("he expect you to go right this time?")

                                print("\nDodge left. (1)")
                                print("Dodge right. (2)")
                                choice = 0
                                while choice != str(1) and choice != str(
                                        2) and choice != "player" and choice != "Player":
                                    choice = input("\nWhat do you do?: ")
                                    if choice == "player" or choice == "Player":
                                        print(new_player.player_stats())
                                        choice = str(0)
                                    elif choice == str(1):
                                        print("\nAnticipating the bandit to try right this time, you dodge to the left. ")
                                        print("However, you jumped right into the path of the arrow impaling you ")
                                        print("on your thigh.")

                                        print("\nYou lose 40 point of health")
                                        new_player.health = new_player.health - 40

                                        print("\nAlthough it hurts like hell he wasn't able to kill you and now you are ")
                                        print("within striking range. You take your sword and quickly cut off one of his arms. ")
                                        print("Screaming in pain and unable to defend himself you cut off his head to end ")
                                        print("this.")

                                        print("\nYou remove the arrow in your leg and patch it up with spare cloth. Afterwards ")
                                        print("you check the bodies of both bandits and find a total of 133 gold pieces. ")
                                        new_player.gold = new_player.gold + 133

                                        print("\nAt least you got something out of this for the trouble. After squaring "
                                              "away your things, you get back on your horse and continue down the road. ")


                                    elif choice == str(2):
                                        print("\nFortunately, you jumped in the right direction once more.")
                                        print("You are now in striking range. You take your sword and quickly cut off one of his arms. ")
                                        print("Screaming in pain and unable to defend himself you cut off his head to end ")
                                        print("this.")

                                        print("\nAfterwards you check the bodies of both bandits and find a total of 133 gold pieces. ")
                                        new_player.gold = new_player.gold + 133

                                        print("\nAt least you got something out of this for the trouble. After squaring "
                                            "away your things, you get back on your horse and continue down the road. ")
                                    else:
                                        print("Invalid option")

                            elif choice == str(2):
                                new_player.health = new_player.health - 60
                                print("\nWith your excellent luck, you jumped right into the path of the arrow. The bandit ")
                                print("anticipated your movement and led his shot. The arrow struck you on the right side of ")
                                print("your chest. ")

                                print("\nYou take 60 points of damage.")

                                print("Hurting and bleeding like hell you keep charging to get close. There's ")
                                print("still time for the bandit to shoot again. He readies his arrow and aims once more.")

                                print("\nDodge left. (1)")
                                print("Dodge right. (2)")
                                choice = 0
                                while choice != str(1) and choice != str(
                                        2) and choice != "player" and choice != "Player":
                                    choice = input("\nWhat do you do?: ")
                                    if choice == "player" or choice == "Player":
                                        print(new_player.player_stats())
                                        choice = str(0)
                                    elif choice == str(1):
                                        new_player.health = new_player.health - 60
                                        print("\nOnce again being unfortunate, the bandit guessed the right direction. ")
                                        print("The arrow pierces your skull and you slump to the ground immediately. ")
                                        print("You could not finish your journey.")

                                        print("\nCause of death: Being bad at gambling.")
                                        quit()

                                    elif choice == str(2):
                                        print("\nFortunately, you jumped in the right direction once more.")
                                        print("You are now in striking range. You take your sword and quickly cut off one of his arms. ")
                                        print("Screaming in pain and unable to defend himself you cut off his head to end ")
                                        print("this.")

                                        print("\nYou remove the arrow in your leg and patch it up with spare cloth. Afterwards ")
                                        print("you check the bodies of both bandits and find a total of 133 gold pieces. ")
                                        new_player.gold = new_player.gold + 133

                                        print("\nAt least you got something out of this for the trouble. After squaring " "away your things, you get back on your horse and continue down the road. ")
                                    else:
                                        print("Invalid option")
                            else:
                                print("Invalid option")


                    elif choice == str(2):
                        print("\nYou try to kill the man trying to kill you. You won't be able to draw your sword in a ")
                        print("kneeling position so you must use the bandit's dagger. You both wrestle over the dagger ")
                        print("but both of you are evenly matched. With both of your hands on the dagger all it can do is ")
                        print("raise above your heads as you fight for it. By doing so the blood from your wound drips into ")
                        print("the bandit's eyes, giving you a momentary advantage. You don't waste it as you let go of the ")
                        print("dagger and get up. With the bandit trying to wipe the blood out of his eyes you draw your ")
                        print("sword and quickly run him through. The bandit dies on the spot.")

                        print("\nYou turn your attention to the other bandit that was sneaking up to your horse. You see ")
                        print("him rifling through your saddlebags! You rush over fast but the bandit notices you and ")
                        print("then tries to steal your horse by jumping on. With all the commotion however, the horse gets ")
                        print("spooked and bucks the bandit off of it. The bandit quickly gets up and runs into thw woods. ")
                        print("Unable to catch up and glad you're alive you let him go. He could be leading you into another ")
                        print("trap. You check over your things and notices all of you gold has been stolen")

                        print("\nYou lost", new_player.gold, "gold pieces.")
                        new_player.gold = new_player.gold - new_player.gold

                        print("\nAngry as you are, you can't do anything about it. You try and justify the situation ")
                        print("with your life, it makes you feel a bit better. You go back to the bandit's body, that's ")
                        print("actually dead this time, to look for any spoils. You check all over this time and find ")
                        print("a small bag of gold coin.")

                        print("\nYou looted 75 gold pieces.")
                        new_player.gold = new_player.gold + 75

                        print("\nAfter squaring away your things, you get back on your horse and continue down the road.")

                    else:
                        print("Invalid option")

            elif choice == str(2):
                print("\nFearing it might be a trap you leave it alone. There could be bandits waiting and watching ")
                print("nearby. You pass it and as you are leaving you hear a painful moaning. Your heart skips a beat.")
                print("The man is still alive! You might be able to help the man or even take him back.")

                print("\nPlease enter the matching number.")

                print("\nIgnore the man and continue on your way. (1)")
                print("Go back and help the man. (2)")
                choice = 0
                while choice != str(1) and choice != str(2) and choice != "player" and choice != "Player":
                    choice = input("\nWhat do you do?: ")
                    if choice == "player" or choice == "Player":
                        print(new_player.player_stats())
                        choice = str(0)

                    elif choice == str(1):
                        print("\nIgnoring the man and fearing the worst you kick your horse into high gear and speed off. ")
                        print("You look back to see the man that was on the ground now standing with another person ")
                        print("coming out of the forest watching you riding off. You just avoid a bandit ambush. ")

                    elif choice == str(2):
                        print("\nYou turn around to go help the man. But being cautious you stay on your horse to inspect ")
                        print("him. You ask him if he's injured. The man replies, 'Yes!, please help me get off your horse ")
                        print("and help me out.' Still watching from your horse, you notice something in the corner of your ")
                        print("eye. It's the shape of a man squatting in the bushes near the woods. 'This is definitely an ambush', ")
                        print("you think to yourself.")

                        print("\nQuickly run away. (1)")
                        print("Hold the man hostage. (2)")
                        choice = 0
                        while choice != str(1) and choice != str(2) and choice != "player" and choice != "Player":
                            choice = input("\nWhat do you do?: ")
                            if choice == "player" or choice == "Player":
                                print(new_player.player_stats())
                                choice = str(0)

                            elif choice == str(1):
                                print("\nYou yell to your horse to turn around and burst into a full gallop. Doing so ")
                                print("causes the man to get up and yell to his partner. It's a bandit attack! ")
                                print("You kick your horse to go as fast as it can. You look back to see that the bandits ")
                                print("shot and arrow at you. Unable to land a direct hit, it grazes your arm and cuts it open.")

                                print("\nYou lose 25 points of health.")
                                new_player.health = new_player.health - 25

                                print("However, you were able to get away and out of the range of the arrows. You keep ")
                                print("riding until you are sure they can't catch up anymore.")

                            elif choice == str(2):
                                print("\nYou get off your horse nonchalantly and walk over to the man. You then quickly ")
                                print("draw your sword and hold him at sword point. You tell him to call his partner out. ")
                                print("As his partner comes out you demand him to hand over all his money. You get the ")
                                print("other guy off the ground and hold the sword by his neck. 'I'll kill him if else!, ")
                                print("you yell at the partner.")

                                print("\nThe partner quickly drops all his money on the road for you. 'That's all we ")
                                print("have he says. Please let him go!'")

                                print("\nLet him go. (1)")
                                print("Kill the man. (2)")
                                choice = 0
                                while choice != str(1) and choice != str(
                                        2) and choice != "player" and choice != "Player":
                                    choice = input("\nWhat do you do?: ")
                                    if choice == "player" or choice == "Player":
                                        print(new_player.player_stats())
                                        choice = str(0)
                                    elif choice == str(1):
                                        print("\nYou let the man go but as soon as you do the man punches you right in ")
                                        print("stomach. Bent over in pain they pick up their money and run off into the ")
                                        print("woods.")

                                        print("\nYou lose 10 points of health.")
                                        new_player.health = new_player.health - 10

                                        print("\nStill in pain you square away your things, you get back on your horse "
                                              "and continue down the road.")

                                    elif choice == str(2):
                                        print("\nYou say, 'ok'. Then slice his throat with your sword. The man falls to ")
                                        print("ground with blood gushing out. His partners screams in terror and runs away ")
                                        print("fearing what you might do to him. You pick up the dropped money and check ")
                                        print("the man's body to find a total of 97 gold pieces.")
                                        new_player.gold = new_player.gold + 97

                                        print("\nAfter squaring away your things, you get back on your horse and continue"
                                              "down thw road.")
                                    else:
                                        print("Invalid option")

                            else:
                                print("Invalid option")
                    else:
                        print("Invalid option")

            else:
                print("Invalid option")

        print("\nYou continue down the main road towards", village_name, ",you are already halfway there thanks to your horse. ")
        print("On your way to the village you notice that the bridge connecting the kingdom from the village has been destroyed. It looks ")
        print("as if the middle of the bridge had a heavy bolder dropped through it. The only other way to go is down the cliff ")
        print("and back up again. However, the path looks dangerous. ")

        print("\nTry jumping the gap. (1)")
        print("Go down side path. (2)")
        choice = 0
        while choice != str(1) and choice != str(2) and choice != "player" and choice != "Player":
            choice = input("\nWhat do you do?: ")
            if choice == "player" or choice == "Player":
                print(new_player.player_stats())
                choice = str(0)

            elif choice == str(1):
                print("\nEither being stupid or brave you back your horse up down the road and begin to gallop at full ")
                print("speed to jump the gap. You kick the horse to go faster as you get closer. As you approach the gap ")
                print("the horse breaks putting it's weight forward and flinging you off in the process. You fly through ")
                print("the air and land on the over side of the bridge. Your horse is stuck on the other side, but with ")
                print("nothing you can do, you thank the horse for it's service and head off.")

                print("\nYou've been walking now for an hour and should be arriving soon to", village_name)

            elif choice == str(2):
                print("\nNot wanting to be a stunt man you head down the side path with your horse. You walk down the ")
                print("side of the cliff with water rushing below. You get to the bottom where the water meets the land. ")
                print("You look around for something to help you get across but all you see are some rocks poking out ")
                print("of the water.")

                print("\nFord the river with your horse. (1)")
                print("Try jumping on the rocks. (2)")
                choice = 0
                while choice != str(1) and choice != str(2) and choice != "player" and choice != "Player":
                    choice = input("\nWhat do you do?: ")
                    if choice == "player" or choice == "Player":
                        print(new_player.player_stats())
                        choice = str(0)

                    elif choice == str(1):
                        print("\nYou don't know how deep the water is but you need to try and get across. Knowing horses ")
                        print("are decent swimmers you try and ford the river. As soon as you jump in you are met with ")
                        print("realization that the current is very strong. Your horse is unable to keep afloat let alone cross. ")
                        print("With your only option, you jump from the back of your horse as a platform and land in the ")
                        print("water near the edge. You pull yourself up as you watch your horse get swept away by the ")
                        print("current. Sadden by this you thank it for its service.")

                        print("\nYou check yourself over only to find out that all your money is gone! It must of been ")
                        print("swept up by the river as well. ")

                        print("\nYou lose", new_player.gold, "gold pieces.")

                        print("\nYou rest a moment to process your recent bad luck before ")
                        print("heading back up the cliff and onto the main road.")
                        new_player.gold = new_player.gold - new_player.gold

                        print("\nYou've been walking now for an hour and should be arriving soon to", village_name)


                    elif choice == str(2):
                        print("\nYou get off your horse and leave him. This is where both of your journeys ends together. ")
                        print("Looking at the rocks there are only a few and spaced far in between. You judge the distance ")
                        print("and the trajectory needed to cross safely. You decide you need to run fast across to keep ")
                        print("your momentum. ")

                        print("\nYou take a few steps back before running. You jump to the first rock with ease not slowing ")
                        print("down. You jump to the second and third, as you jump from the last rock your foot slips ")
                        print("from the frictionless surface and you end up tumbling onto your head on the other side.")

                        print("\nYou take 40 points of damage.")
                        new_player.health = new_player.health - 40
                        if new_player.health > 0:
                            print("\nWith a concussion now, you rest a bit before heading back up the cliff. You get "
                                  "back on the main path and head towards", village_name)
                            print("\nIts been an hour walking now and should be arriving soon.")

                        else:
                            print("\nAlready in great pain from before, this concussion proved to be too much for you.")
                            print("\nCause of death: Head Injury.")
                    else:
                        print("Invalid option")

            else:
                print("Invalid option")


    else:
        print("Invalid option")







# End Sequence
print("\nYou look ahead and see something on the side of the road, it's a traveling merchant! He has his wares set up ")
print("for display.")


print("\nGo check out the wares. (1)")
print("Continue and walk past. (2)")
choice = 0
while choice != str(1) and choice != str(2) and choice != "player" and choice != "Player":
    choice = input("\nWhat do you do?: ")
    if choice == "player" or choice == "Player":
        print(new_player.player_stats())
        choice = str(0)

    elif choice == str(1):
        print("\nYou decide to check out the merchants wears. As you approach you hear the merchant talking to ")
        print("you, 'Welcome, Welcome! Please won't you stop by and look?' You greet the merchant and look over ")
        print("his goods. Much of his wares are useless to you, however, you spot a few that are of use. You see ")
        print("a shiny sword that looks to be crafted with care, a medicinal pouch, and some rope. He tells you, ")
        print("'I must warn you, I will only sell one item to you'.")

        print("\nAsk about sword. (1)")
        print("Ask about medicinal pouch. (2)")
        print("Ask about rope. (3)")
        print("Walk away. (4)")
        choice = 0
        while choice != str(1) and choice != str(2) and choice != str(3) and choice != str(4) and choice != "player" and choice != "Player":
            choice = input("\nWhat do you do?: ")
            if choice == "player" or choice == "Player":
                print(new_player.player_stats())
                choice = str(0)

            elif choice == str(1):
                print("\n'Tell me about this sword' you say. 'Oh that?' says the merchant. 'This right here is ")
                print("a high quality sword. This was crafted in the far land to the east by master ")
                print("craftsmen. I got it from one my journeys over there. It's said to be able to cut through ")
                print("any type of armor.'. Interested you ask for the price. 'I'll sell it for 200 gold pieces.'. ")

                print("\nBuy sword. (1)")
                print("Pass on it. (2)")
                choice = 0
                while choice != str(1) and choice != str(2) and choice != "player" and choice != "Player":
                    choice = input("\nWhat do you do?: ")
                    if choice == "player" or choice == "Player":
                        print(new_player.player_stats())
                        choice = str(0)

                    elif choice == str(1):
                        if new_player.gold > 199:
                            print(
                                "\nYou hand over the gold and receive your new sword, it feels like it's been crafted ")
                            print("with a lot of care.")

                            print("\nYou loose 200 gold pieces.")
                            new_player.gold = new_player.gold - 200
                            print("Your attack strength as gone up.")
                            new_player.attack_power = new_player.attack_power + 0.8
                            print("\nThank you for your purchase. Goodbye now.")

                        else:
                            print("Disgusted by your lack of money and wasting his time. He tells you to get lost.")

                    elif choice == str(2):
                        print("\n'Something else then?'")

                        print("\nThe medicinal pouch. (1)")
                        print("The rope. (2)")
                        print("Leave. (3)")
                        choice = 0
                        while choice != str(1) and choice != str(2) and choice != str(
                                3) and choice != "player" and choice != "Player":
                            choice = input("\nWhat do you do?: ")
                            if choice == "player" or choice == "Player":
                                print(new_player.player_stats())
                                choice = str(0)
                            elif choice == str(1):
                                print("\nYou ask about the medicinal pouch. 'Oh that, this is a pouch full of ")
                                print("medicinal herbs that will help treat any aliments one might need. I'll ")
                                print("sell it for 50 Gold pieces.'")

                                print("\nBuy pouch. (1)")
                                print("Pass on it. (2)")
                                choice = 0
                                while choice != str(1) and choice != str(
                                        2) and choice != "player" and choice != "Player":
                                    choice = input("\nWhat do you do?: ")
                                    if choice == "player" or choice == "Player":
                                        print(new_player.player_stats())
                                        choice = str(0)

                                    elif choice == str(1):
                                        if new_player.gold > 49:
                                            print("\nYou trade the gold for the pouch and use it right away. ")
                                            print("You are healed for 50 points of health.")
                                            new_player.health = new_player.health + 50
                                            print("Thank you for your purchase. Goodbye now!")
                                        else:
                                            print(
                                                "\nDisgusted by your lack of money and wasting his time. He tells you to get lost.")

                                    elif choice == str(2):
                                        print("\n'Something else then?'")

                                        print("\nThe rope. (1)")
                                        print("Leave. (2)")
                                        choice = 0
                                        while choice != str(1) and choice != str(
                                                2) and choice != "player" and choice != "Player":
                                            choice = input("\nWhat do you do?: ")
                                            if choice == "player" or choice == "Player":
                                                print(new_player.player_stats())
                                                choice = str(0)

                                            elif choice == str(1):
                                                print("\nYou ask about the rope. 'Standard rope' he says, 'About ")
                                                print("15 meters in length. I'll sell it for 25 gold pieces.'")

                                                print("\nBuy rope. (1)")
                                                print("Leave. (2)")
                                                choice = 0
                                                while choice != str(1) and choice != str(
                                                        2) and choice != "player" and choice != "Player":
                                                    choice = input("\nWhat do you do?: ")
                                                    if choice == "player" or choice == "Player":
                                                        print(new_player.player_stats())
                                                        choice = str(0)

                                                    elif choice == str(1):
                                                        if new_player.gold > 24:
                                                            print("You hand over the gold and receive the rope. ")
                                                            print("It could come handy in the future.")
                                                        else:
                                                            print("The merchant laughs at you for being poor"
                                                                  "and tells you to get lost.")

                                                    elif choice == str(2):
                                                        print("\nYou change your mind and leave.")
                                                    else:
                                                        print("Invalid option")

                                            elif choice == str(2):
                                                print("\nYou change your mind and leave the merchant.")
                                            else:
                                                print("Invalid option")
                                    else:
                                        print("Invalid option")

                            elif choice == str(2):
                                print("\nYou ask about the rope. 'Standard rope' he says, 'About ")
                                print("15 meters in length. I'll sell it for 25 gold pieces.'")

                                print("\nBuy rope. (1)")
                                print("Pass on it. (2)")
                                choice = 0
                                while choice != str(1) and choice != str(
                                        2) and choice != "player" and choice != "Player":
                                    choice = input("\nWhat do you do?: ")
                                    if choice == "player" or choice == "Player":
                                        print(new_player.player_stats())
                                        choice = str(0)

                                    elif choice == str(1):
                                        if new_player.gold > 24:
                                            print("You hand over the gold and receive the rope. It ")
                                            print("could come handy in the future.")
                                        else:
                                            print("The merchant laughs at you for being poor"
                                                  "and tells you to get lost.")

                                    elif choice == str(2):
                                        print("\n'Something else then?'")

                                        print("\nMedicinal pouch. (1)")
                                        print("Leave. (2)")
                                        choice = 0
                                        while choice != str(1) and choice != str(
                                                2) and choice != "player" and choice != "Player":
                                            choice = input("\nWhat do you do?: ")
                                            if choice == "player" or choice == "Player":
                                                print(new_player.player_stats())
                                                choice = str(0)
                                            elif choice == str(1):
                                                print("\nYou ask about the medicinal pouch. 'Oh that, this is a pouch full of ")
                                                print("medicinal herbs that will help treat any aliments one might need. I'll ")
                                                print("sell it for 50 Gold pieces.'")

                                                print("\nBuy pouch. (1)")
                                                print("Pass on it. (2)")
                                                choice = 0
                                                while choice != str(1) and choice != str(2) and choice != "player" and choice != "Player":
                                                    choice = input("\nWhat do you do?: ")
                                                    if choice == "player" or choice == "Player":
                                                        print(new_player.player_stats())
                                                        choice = str(0)

                                                    elif choice == str(1):
                                                        if new_player.gold > 49:
                                                            print("\nYou trade the gold for the pouch and use it right away. ")
                                                            print("You are healed for 50 points of health.")
                                                            new_player.health = new_player.health + 50
                                                            print("Thank you for your purchase. Goodbye now!")
                                                        else:
                                                            print("\nDisgusted by your lack of money and wasting his time. He tells you to get lost.")

                                                    elif choice == str(2):
                                                        print("\nYou decide not to buy anything and leave.")
                                                    else:
                                                        print("Invalid option")

                                            elif choice == str(2):
                                                print("\nYou decide not to buy anything and leave.")
                                            else:
                                                print("Invalid option")

                                    else:
                                        print("Invalid option")

                            elif choice == str(3):
                                print("\nYou changed your mind and leave the merchant.")
                            else:
                                print("Invalid option")
                    else:
                        print("Invalid option")



            elif choice == str(2):
                print("\nYou ask about the medicinal pouch. 'Oh that, this is a pouch full of ")
                print("medicinal herbs that will help treat any aliments one might need. I'll ")
                print("sell it for 50 Gold pieces.'")

                print("\nBuy pouch. (1)")
                print("Pass on it. (2)")
                choice = 0
                while choice != str(1) and choice != str(2) and choice != "player" and choice != "Player":
                    choice = input("\nWhat do you do?: ")
                    if choice == "player" or choice == "Player":
                        print(new_player.player_stats())
                        choice = str(0)

                    elif choice == str(1):
                        if new_player.gold > 49:
                            print("\nYou trade the gold for the pouch and use it right away. ")
                            print("You are healed for 50 points of health.")
                            new_player.health = new_player.health + 50
                            print("Thank you for your purchase. Goodbye now!")
                        else:
                            print("\nDisgusted by your lack of money and wasting his time. He tells you to get lost.")

                    elif choice == str(2):
                        print("\n'Something else then?'")

                        print("\nThe sword. (1)")
                        print("The rope. (2)")
                        print("Leave. (3)")
                        choice = 0
                        while choice != str(1) and choice != str(2) and choice != str(
                                3) and choice != "player" and choice != "Player":
                            choice = input("\nWhat do you do?: ")
                            if choice == "player" or choice == "Player":
                                print(new_player.player_stats())
                                choice = str(0)

                            elif choice == str(1):
                                print(
                                    "\n'Tell me about this sword' you say. 'Oh that?' says the merchant. 'This right here is ")
                                print(
                                    "a high quality sword. This was crafted in the far land to the east by master ")
                                print(
                                    "craftsmen. I got it from one my journeys over there. It's said to be able to cut through ")
                                print(
                                    "any type of armor.'. Interested you ask for the price. 'I'll sell it for 200 gold pieces.'. ")

                                print("\nBuy sword. (1)")
                                print("Pass on it. (2)")
                                choice = 0
                                while choice != str(1) and choice != str(
                                        2) and choice != "player" and choice != "Player":
                                    choice = input("\nWhat do you do?: ")
                                    if choice == "player" or choice == "Player":
                                        print(new_player.player_stats())
                                        choice = str(0)

                                    elif choice == str(1):
                                        if new_player.gold > 199:
                                            print(
                                                "\nYou hand over the gold and receive your new sword, it feels like it's been crafted ")
                                            print("with a lot of care.")

                                            print("\nYou loose 200 gold pieces.")
                                            new_player.gold = new_player.gold - 200
                                            print("Your attack strength as gone up.")
                                            new_player.attack_power = new_player.attack_power + 0.8
                                            print("\nThank you for your purchase. Goodbye now.")

                                        else:
                                            print(
                                                "Disgusted by your lack of money and wasting his time. He tells you to get lost.")

                                    elif choice == str(2):
                                        print("\n'Something else then?'")

                                        print("\nThe Rope. (1)")
                                        print("Leave. (2)")
                                        choice = 0
                                        while choice != str(1) and choice != str(
                                                2) and choice != "player" and choice != "Player":
                                            choice = input("\nWhat do you do?: ")
                                            if choice == "player" or choice == "Player":
                                                print(new_player.player_stats())
                                                choice = str(0)

                                            elif choice == str(1):
                                                print("\nYou ask about the rope. 'The Rope? Its just your standard ")
                                                print("rope' he says. '15 meters long, I'll sell it to you for ")
                                                print("gold pieces.'")

                                                print("\nBuy rope. (1)")
                                                print("Pass on it. (2)")
                                                choice = 0
                                                while choice != str(1) and choice != str(
                                                        2) and choice != "player" and choice != "Player":
                                                    choice = input("\nWhat do you do?: ")
                                                    if choice == "player" or choice == "Player":
                                                        print(new_player.player_stats())
                                                        choice = str(0)

                                                    elif choice == str(1):
                                                        if new_player.gold > 24:
                                                            print("You hand over the gold and receive the rope. It ")
                                                            print("could come handy in the future.")
                                                            new_player.gold = new_player.gold - 25
                                                            print("Thank you for your purchase. Goodbye!")

                                                        else:
                                                            print("The merchant laughs at you for being poor"
                                                                  "and tells you to get lost.")

                                                    elif choice == str(2):
                                                        print("\nYou decide not to buy anything and leave.")
                                                    else:
                                                        print("Invalid option")

                                            elif choice == str(2):
                                                print("\nYou decide not to buy anything and leave.")
                                            else:
                                                print("Invalid option")

                                    else:
                                        print("Invalid option")

                            elif choice == str(2):
                                print("\nYou ask about the rope. 'Standard rope' he says, 'About ")
                                print("15 meters in length. I'll sell it for 25 gold pieces.'")

                                print("\nBuy rope. (1)")
                                print("Pass on it. (2)")
                                choice = 0
                                while choice != str(1) and choice != str(
                                        2) and choice != "player" and choice != "Player":
                                    choice = input("\nWhat do you do?: ")
                                    if choice == "player" or choice == "Player":
                                        print(new_player.player_stats())
                                        choice = str(0)

                                    elif choice == str(1):
                                        if new_player.gold > 24:
                                            print("You hand over the gold and receive the rope. It ")
                                            print("could come handy in the future.")
                                            new_player.gold = new_player.gold - 25
                                            print("Thank you for your purchase. Goodbye!")

                                        else:
                                            print("The merchant laughs at you for being poor"
                                                  "and tells you to get lost.")

                                    elif choice == str(2):
                                        print("\n'Something else then?'")

                                        print("\nThe Sword. (1)")
                                        print("Leave. (2)")
                                        choice = 0
                                        while choice != str(1) and choice != str(
                                                2) and choice != "player" and choice != "Player":
                                            choice = input("\nWhat do you do?: ")
                                            if choice == "player" or choice == "Player":
                                                print(new_player.player_stats())
                                                choice = str(0)

                                            elif choice == str(1):
                                                print(
                                                    "\n'Tell me about this sword' you say. 'Oh that?' says the merchant. 'This right here is ")
                                                print(
                                                    "a high quality sword. This was crafted in the far land to the east by master ")
                                                print(
                                                    "craftsmen. I got it from one my journeys over there. It's said to be able to cut through ")
                                                print(
                                                    "any type of armor.'. Interested you ask for the price. 'I'll sell it for 200 gold pieces.'. ")

                                                print("\nBuy sword. (1)")
                                                print("Pass on it. (2)")
                                                choice = 0
                                                while choice != str(1) and choice != str(
                                                        2) and choice != "player" and choice != "Player":
                                                    choice = input("\nWhat do you do?: ")
                                                    if choice == "player" or choice == "Player":
                                                        print(new_player.player_stats())
                                                        choice = str(0)

                                                    elif choice == str(1):
                                                        if new_player.gold > 199:
                                                            print(
                                                                "\nYou hand over the gold and receive your new sword, it feels like it's been crafted ")
                                                            print("with a lot of care.")

                                                            print("\nYou lose 200 gold pieces.")
                                                            new_player.gold = new_player.gold - 200
                                                            print("Your attack strength as gone up.")
                                                            new_player.attack_power = new_player.attack_power + 0.8
                                                            print("\nThank you for your purchase. Goodbye now.")

                                                        else:
                                                            print("Disgusted by your lack of money and wasting his time. He tells you to get lost.")

                                                    elif choice == str(2):
                                                        print("\nYou decide not to buy anything and leave.")
                                                    else:
                                                        print("Invalid option")

                                            elif choice == str(2):
                                                print("\nYou decide not to buy anything and leave.")
                                            else:
                                                print("Invalid option")

                                    else:
                                        print("Invalid option")

                            elif choice == str(3):
                                print("\nYou changed your mind and leave.")

                            else:
                                print("Invalid option")

                    else:
                        print("Invalid option")

            elif choice == str(3):
                print("\nYou ask about the rope. 'Standard rope' he says, 'About ")
                print("15 meters in length. I'll sell it for 25 gold pieces.'")

                print("\nBuy rope. (1)")
                print("Pass on it. (2)")
                choice = 0
                while choice != str(1) and choice != str(2) and choice != "player" and choice != "Player":
                    choice = input("\nWhat do you do?: ")
                    if choice == "player" or choice == "Player":
                        print(new_player.player_stats())
                        choice = str(0)
                    elif choice == str(1):
                        if new_player.gold > 24:
                            print("You hand over the gold and receive the rope. It ")
                            print("could come handy in the future.")
                            new_player.gold = new_player.gold - 25
                            print("Thank you for your purchase. Goodbye!")

                        else: print("The merchant laughs at you for being poor and tells you to get lost.")

                    elif choice == str(2):
                        print("\n'Something else then?'")

                        print("\nThe sword. (1)")
                        print("The medicinal pouch. (2)")
                        print("Leave. (3)")
                        choice = 0
                        while choice != str(1) and choice != str(2) and choice != str(3) and choice != "player" and choice != "Player":
                            choice = input("\nWhat do you do?: ")
                            if choice == "player" or choice == "Player":
                                print(new_player.player_stats())
                                choice = str(0)
                            elif choice == str(1):
                                print("\n'Tell me about this sword' you say. 'Oh that?' says the merchant. 'This right here is ")
                                print("a high quality sword. This was crafted in the far land to the east by master ")
                                print("craftsmen. I got it from one my journeys over there. It's said to be able to cut through ")
                                print("any type of armor.'. Interested you ask for the price. 'I'll sell it for 200 gold pieces.'. ")

                                print("\nBuy sword. (1)")
                                print("Pass on it. (2)")
                                choice = 0
                                while choice != str(1) and choice != str(
                                        2) and choice != "player" and choice != "Player":
                                    choice = input("\nWhat do you do?: ")
                                    if choice == "player" or choice == "Player":
                                        print(new_player.player_stats())
                                        choice = str(0)

                                    elif choice == str(1):
                                        if new_player.gold > 199:
                                            print("\nYou hand over the gold and receive your new sword, it feels like ")
                                            print("it's been crafted with a lot of care.")

                                            print("\nYou lose 200 gold pieces.")
                                            new_player.gold = new_player.gold - 200
                                            print("Your attack strength as gone up.")
                                            new_player.attack_power = new_player.attack_power + 0.8
                                            print("\nThank you for your purchase. Goodbye now.")

                                        else:
                                            print("Disgusted by your lack of money and wasting his time. He tells you to get lost.")

                                    elif choice == str(2):
                                        print("\n'Something else then?'")

                                        print("\nThe medicinal pouch. (1)")
                                        print("Leave. (2)")
                                        choice = 0
                                        while choice != str(1) and choice != str(
                                                2) and choice != "player" and choice != "Player":
                                            choice = input("\nWhat do you do?: ")
                                            if choice == "player" or choice == "Player":
                                                print(new_player.player_stats())
                                                choice = str(0)

                                            elif choice == str(1):
                                                print("\nYou ask about the medicinal pouch. 'Oh that, this is a pouch full of ")
                                                print("medicinal herbs that will help treat any aliments one might need. I'll ")
                                                print("sell it for 50 Gold pieces.'")

                                                print("\nBuy pouch. (1)")
                                                print("Pass on it. (2)")
                                                choice = 0
                                                while choice != str(1) and choice != str(
                                                        2) and choice != "player" and choice != "Player":
                                                    choice = input("\nWhat do you do?: ")
                                                    if choice == "player" or choice == "Player":
                                                        print(new_player.player_stats())
                                                        choice = str(0)
                                                    elif choice == str(1):
                                                        if new_player.gold > 49:
                                                            print(
                                                                "\nYou trade the gold for the pouch and use it right away. ")
                                                            print("You are healed for 50 points of health.")
                                                            new_player.health = new_player.health + 50
                                                            print("Thank you for your purchase. Goodbye now!")
                                                        else:
                                                            print("\nDisgusted by your lack of money and wasting his time. He tells you to get lost.")

                                                    elif choice == str(2):
                                                        print("\nYou decide not to buy anything and leave.")
                                                    else:
                                                        print("Invalid option")

                                            elif choice == str(2):
                                                print("\nYou decide to buy nothing and leave.")
                                            else:
                                                print("Invalid option")
                                    else:
                                        print("Invalid option")


                            elif choice == str(2):
                                print("\nYou ask about the medicinal pouch. 'Oh that, this is a pouch full of ")
                                print("medicinal herbs that will help treat any aliments one might need. I'll ")
                                print("sell it for 50 Gold pieces.'")

                                print("\nBuy pouch. (1)")
                                print("Pass on it. (2)")
                                choice = 0
                                while choice != str(1) and choice != str(
                                        2) and choice != "player" and choice != "Player":
                                    choice = input("\nWhat do you do?: ")
                                    if choice == "player" or choice == "Player":
                                        print(new_player.player_stats())
                                        choice = str(0)
                                    elif choice == str(1):
                                        if new_player.gold > 49:
                                            print(
                                                "\nYou trade the gold for the pouch and use it right away. ")
                                            print("You are healed for 50 points of health.")
                                            new_player.health = new_player.health + 50
                                            print("Thank you for your purchase. Goodbye now!")
                                        else:
                                            print("\nDisgusted by your lack of money and wasting his time. He tells you to get lost.")

                                    elif choice == str(2):
                                        print("\n'Something else then?'")

                                        print("\nThe sword. (1)")
                                        print("Leave. (2)")
                                        choice = 0
                                        while choice != str(1) and choice != str(
                                                2) and choice != "player" and choice != "Player":
                                            choice = input("\nWhat do you do?: ")
                                            if choice == "player" or choice == "Player":
                                                print(new_player.player_stats())
                                                choice = str(0)
                                            elif choice == str(1):
                                                print("\n'Tell me about this sword' you say. 'Oh that?' says the merchant. 'This right here is ")
                                                print("a high quality sword. This was crafted in the far land to the east by master ")
                                                print("craftsmen. I got it from one my journeys over there. It's said to be able to cut through ")
                                                print("any type of armor.'. Interested you ask for the price. 'I'll sell it for 200 gold pieces.'. ")

                                                print("\nBuy sword. (1)")
                                                print("Pass on it. (2)")
                                                choice = 0
                                                while choice != str(1) and choice != str(
                                                        2) and choice != "player" and choice != "Player":
                                                    choice = input("\nWhat do you do?: ")
                                                    if choice == "player" or choice == "Player":
                                                        print(new_player.player_stats())
                                                        choice = str(0)
                                                    elif choice == str(1):
                                                        if new_player.gold > 199:
                                                            print("\nYou hand over the gold and receive your new sword, it feels like ")
                                                            print("it's been crafted with a lot of care.")

                                                            print("\nYou lose 200 gold pieces.")
                                                            new_player.gold = new_player.gold - 200
                                                            print("Your attack strength as gone up.")
                                                            new_player.attack_power = new_player.attack_power + 0.8
                                                            print("\nThank you for your purchase. Goodbye now.")

                                                        else:
                                                            print("Disgusted by your lack of money and wasting his time. He tells you to get lost.")

                                                    elif choice == str(2):
                                                        print("\nYou decide not to buy anything and leave.")
                                                    else:
                                                        print("Invalid option")

                                            elif choice == str(2):
                                                print("\nYou decide not to buy anything and leave")
                                            else:
                                                print("Invalid option")
                                    else:
                                        print("Invalid option")

                            elif choice == str(3):
                                print("You decide to not buy anything and leave.")
                            else:
                                print("Invalid option")
                    else:
                        print("Invalid option")

            elif choice == str(4):
                print("\nAfter looking over the goods you changed your mind and leave the merchant.")

            else: print("Invalid option")

        break
    elif choice == str(2):
        print("\nNot wanting to spend any money, you continue towards", village_name)
    else:
        print("Invalid option")

print("\nAs you walk you notice the road starting to become more structured and paved with stone. In the distance ")
print("you see the front gate of", village_name, "from the outside you don't notice anything weird. However, as ")
print("you watch, you do not see a single person or form of life moving, it's eerily quite and still. ")
print("You approach with caution as you have good reason to. You finally reached your destination and the source ")
print("of your quest. Who knows what you'll find in here. You'll soon discover why you were sent here.")

print("\nContinue the journey in part 2. Thank you for playing this far!")
quit()