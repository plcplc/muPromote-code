-- | This module hosts definitions describing a software project as an
-- organisation of people developing a piece (or suite) of software.
module MuPromote.Common.Package.Project where

-- | Here, a Project is something that is eligible as a receiver of donations.
-- A project may host multiple software packages.  This data type should
-- contain contact information and should probably reside inside the Server
-- namespace rather than the Common namespace.
data Project = ProjectOrganisation -- e.g. KDE, Gnome..  ProjectIndividual   --
-- e.g. Peter Jackson, Linus Thorvalds..
