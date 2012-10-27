"""
I wanted a knife that had the tools that I really wanted, maybe some tools that could come in handy, but not an excess of unwanted tools.
"""

from itertools import imap, ifilter
import re


def clean_tool(tool):
    """
..function::clean_tool(tool : str) -> str

Normalizes a tool's name

"""

    replacements = [
        (r"([^\w\s]|with)+", ""),
        (r"philips", "phillips"),
        (r"cap lifter", "bottle opener"),
        (r"^screwdriver$",  "flathead screwdriver"),
        (r"small screwdriver", "flathead screwdriver"),
        (r"philips", "phillips"),
        (r"large lock blade for one hand", "large blade"),
        ]

    return reduce(
        lambda val, (pat, repl): re.sub(pat, repl, val),
        replacements,
        tool.lower(),
    ).strip()


def knife(name, tools):
    """
..function::knife(name : str, tools : set) -> dict

This is what a knife dict looks like
    """
    return {
        "name": name,
        "tools": tools
    }


def tools(text):
    """
Converts a copy/pasted list of tools from the Victorinox product page
into a normalized set of tools
"""

    return set(
        ifilter(
            None,
            imap(
                clean_tool,
                text.split("\n")
            )
        )
    )

def score(ideal_toolset, maybe_toolset, knife):
    """
..function::score(ideal_toolset : set(), maybe_toolset : set(),
                  knife : knife()) -> float

Scores a knife based on my ideal_toolset, the maybe_toolset
dings the knife for any extra unwanted tools.
    """
    # always 0 for knives are missing from the ideal toolset
    if not ideal_toolset.issubset(knife['tools']):
        return -1
    else:
        required_toolset = ideal_toolset & knife['tools']
        bonus_toolset = knife['tools'] & maybe_toolset
        unwanted_toolset = knife['tools'] - (ideal_toolset | maybe_toolset)

        return (
            len(required_toolset) * 1.0
            + len(bonus_toolset) * 0.5
            - len(unwanted_toolset) * 0.1
        )

def get_knife(knives, name):
    """
..function::get_knife(knives : [knife()], name : str) -> knife()

Searches a list of knives for a knife by name
"""
    for knife in knives:
        if knife['name'] == name:
            return knife
####
## Data
####

ideal = {
    "flathead screwdriver",
    "phillips screwdriver",
    "toothpick",
    "large blade",
    "bottle opener",
    "wire stripper",
}

maybe = {
    "wood saw",
    "wire cutters",
    "tweezers",
    "small blade",
    "wire crimping tool",
    "wire cutters",
    "can opener",
    "reamer",
    "key ring",
}


knives = [
    knife("mechanic",
          tools("""
large blade
small blade
cap lifter with
-screwdriver
-wire stripper
reamer
toothpick
pliers
wire cutters
wire crimping tool
Philips screwdriver
can opener with
-small screwdriver
 """)),

    knife("tinker",
          tools("""
large blade
small blade
phillips screwdriver
can opener with
-small screwdriver
bottle opener
wire stripper
reamer
key ring
tweezers
toothpick
""")),

    knife("deluxe tinker",
          tools("""
large blade
small blade
phillips screwdriver
cap lifter with
-screwdriver
-wire stripper
reamer
toothpick
scissors
multi-purpose hook
pliers """)),

    knife("adventurer",
          tools("""
large lock blade for one hand
phillips screwdriver
cap lifter with
-screwdriver
-wire stripper
reamer
toothpick
key ring  """)),

    knife("hiker",
          tools("""
large blade
small blade
phillips screwdriver
cap lifter with
-screwdriver
-wire stripper
reamer
toothpick
wood saw""")),

    knife("Dual Pro X",
          tools("""
large lock blade for one hand
one hand belt cutter
phillips screwdriver
cap lifter with
-screwdriver
-wire stripper
one hand belt cutter""")),
    knife("One-Hand Trekker NS",
          tools("""
large lock blade for one hand
phillips screwdriver
cap lifter with
-screwdriver
-wire stripper
reamer
toothpick
wood saw """)),

]

####
## Do work
####

def print_scored(scored, line_width):
    template = "{knife[name]:<{name_len}}\t{score:g} {missing} {extra} {excess}"

    name_len = max(
        imap(
            lambda (_, knife): len(knife['name']),
            scored
        )
    )

    for score, knife in sorted(scored, reverse=True):
        missing = ideal - knife['tools']
        extra = knife['tools'] & maybe
        excess = knife['tools'] - (ideal | maybe)
        print(
            template.format(
                name_len=name_len,
                diff=name_len,
                score=score,
                knife=knife,
                missing=list(missing),
                extra=list(extra),
                excess=list(excess))
        )


scored_gen = imap(
    lambda knife: (score(ideal, maybe, knife), knife),
    knives
)


print_scored(
    # materialize the scored generator
    list(scored_gen),
    30
)
