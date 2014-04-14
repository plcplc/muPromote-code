μPromote Node
=============

This folder hosts the components that make up the μPromote node software
package.

User scenarios
==============

    The user installs and configures a usage collector, eg a music scrobbler
    plugin for his favourite music player application.

    When he plays music, the scrobbler plugin ambiently transmits a promotable
    item for each song that plays, with a corresponding weight of how much of
    the track was actually played.

    Each week, the node transmits the collected promotable items and
    corresponding weights to the donation processor, and authorizes the
    transfer of EUR 1.0.

An intended usage is that usage collectors periodically present promotable
items for the node. At some point in time, either periodically or by initiative
of the user, the node commits to actually sending some funds to the items by
sending a funding request to the donation processors mentioned in the presented
items.

To be able to differentiate the amount given to each item they can be supplied
with a weight at presentation time.

    The user encounters some promotable item (eg a web comic) and wants to
    promote it because of the quality he gets from it (maybe it cracks him up).

    The site also hosts promotable items for each cartoon, and by inputting
    that to the node (either manually or through some aided means like a
    browser extension) the item is registered for the next promotion iteration.

    The user is also given the option to donate right away together with the
    information of the fees incurred.

Another intended usage is that the user manually presents a promotable item and
a weight.

The above scenarios give rise to two interfaces to the node: An administrative
tool for humans that display a GUI, and one for usage collectors that expose a
JSON web service.

Additionally, it prompts the use of a web service interface to the donation
processor that supports placing donations, and in the name of transparency the
ability to reflect the promotion history of the user and the ability to reflect
on the fee structure.
