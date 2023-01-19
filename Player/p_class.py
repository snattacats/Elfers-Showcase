import random
from Names.Name_Gen import random_name

choice = (1, 2)


class Player:
    health = int(100)  # How much life player has left
    gold = int(0)  # Gold is the currency in this game
    attack_power = float(1.0)  # The number multiplier for attacking
    name = ""   # Name of character
    age = int(18)   # Age of Character
    weight = int(0)     # Weight of character
    sex = True, False   # Gender of character

    def __init__(self):
        self.health = 100
        self.gold = 0
        self.attack_power = 1.0
        self.name = ""
        self.age = 18
        self.weight = 0
        self.sex = True

    def player_stats(self):
        return "\nPlayer Name: {0}\nRemaining Health: {1}\nGold Available: {2}\nCurrent Attack Power: {3}\n" \
               "Age {4}\nWeight: {5}lbs\nSex: {6}\n".format(self.name, self.health, self.gold, self.attack_power,
                                                            self.age, self.weight, self.sex)


def create_player_random():
    player = Player()
    player.health = 100
    player.gold = random.randint(15, 300)
    player.attack_power = 1.0
    player.name = random_name()
    player.age = random.randint(18, 100)
    player.weight = random.randint(75, 350)
    rand = random.choice(choice)
    if rand == 1:
        player.sex = "Male"
    else:
        player.sex = "Female"
    return player


def create_player(name, age, wgt, sex):
    player = Player()
    player.name = name
    player.age = age
    player.weight = wgt
    player.sex = sex
    player.health = 100
    player.gold = random.randint(15, 300)
    player.attack_power = 1.0
    return player


new_player = Player()
