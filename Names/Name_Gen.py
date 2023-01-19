import random


first_syl = ("cur", "gor", "xur", "jay", "ung", "ein", "cor", "kez", "mel", "jar", "li", "vi", "as", "por", "wal", "ha"
             , "do", "al'", "tan")

middle_syl = ("es", "lo", "el", "ah", "oo", "ie", "in", "id",
             "ur", "ein", "es", "li", "ey", "be", "ce", "de", "ef", "ge", "ha", "a", "e",
              "i", "o", "u", "y")

last_syl = ("veon", "amin", "ser", "kae", "ien", "hor", "des", "ol", "ung", "ust", "op", "ath",
            "ink", "ia", "or", "ion", "in", "va", "gi", "ong")



def random_name():
    name = ""
    num_of_syll = random.randint(1, 4)
    if num_of_syll < 4:
        name = name + random.choice(first_syl) + random.choice(middle_syl) + random.choice(last_syl)
    else:
        name = name + random.choice(first_syl) + random.choice(middle_syl) + random.choice(middle_syl) + random.choice(last_syl)
    return str(name.capitalize())


