{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE UnicodeSyntax   #-}

module Brian.EntryData
  ( ets
  ) where

import Base1T

-- text --------------------------------

import Data.Text qualified as T

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Brian.Day qualified as Day

import Brian.Description ( Description(Description) )
import Brian.Entry       ( Entry(Entry, _actresses, _description, _entryDate, _episode, _medium, _recordNumber, _tags, _title) )
import Brian.Episode     ( mkEpisode )
import Brian.ID          ( ID(ID) )
import Brian.Medium      ( Medium(Movie, SoapOpera, TVSeries) )
import Brian.Title       ( Title(Title) )

--------------------------------------------------------------------------------

t1 ∷ 𝕋
t1 = T.unlines [ "Record number: 1"
               , "Title: Guiding Light"
               , "Medium: Soap Opera"
               , "Actress: Sherry Stringfield"
               , "Description: Aired December of 1990."
               , T.unwords [ "Stringfield is kidnapped and held for"
                           , "ransom by her ex. Tied to a" ]
               , T.unwords [ "chair and gagged with white cloth"
                           , "between the teeth. Several good"
                           , "closeups. Ungagged for a phone call,"
                           , "then regagged on screen."
                           ]
               , T.unwords [ "Tags: country_us, gagtype_cleave,"
                           , "bonddesc_chair, onscreen_gagging" ]
               ]

--------------------

e1 ∷ Entry
e1 = Entry { _recordNumber = ID 1
           , _title = "Guiding Light"
           , _medium = 𝕵 SoapOpera
           , _actresses = ["Sherry Stringfield"]
           , _description = Description $
             T.unlines [ "Aired December of 1990."
                       , T.unwords [ "Stringfield is kidnapped and held for"
                                   , "ransom by her ex. Tied to a" ]
                       , T.unwords [ "chair and gagged with white cloth between"
                                   , "the teeth. Several good closeups."
                                   , "Ungagged for a phone call, then regagged"
                                   , "on screen."
                                   ]
                       ]
             , _tags = [ "country_us", "gagtype_cleave" , "bonddesc_chair"
                       , "onscreen_gagging"]
             , _episode = 𝕹
             , _entryDate = Day.epoch
             }

----------------------------------------

t3 ∷ 𝕋
t3 = T.unlines [ "Record number: 3"
               , "Title: The Amazing Spider-Man (1978) aka Spiderman"
               , "Medium: TV Series"
               , "Actress: Madeleine Stowe"
               , T.unwords [ "Description: Episode: \"Escort to"
                           , "Danger\" (1.06)" ]
               , T.unwords [ "As a kidnapped foreign"
                           , "princess, she is kidnapped by"
                           , "terrorists. She." ]
               , T.unwords [ "is sitting in a warehouse talking to one"
                           , "of her captors, and is" ]
               , T.unwords [ "gagged with white cloth between the"
                           , "teeth (on screen). Short scene," ]
               , T.unwords [ "but some pretty good closeups. She is"
                           , "wearing a purple sleeveless" ]
               , T.unwords [ "gown." ]
               , T.unwords [ "Tags: bonddesc_anklestogether,"
                           , "bonddesc_handsbehind, gagtype_cleave,"
                           , "onscreen_gagging, onscreen_tying,"
                           , "outfit_skirt, restraint_rope, country_us"
                           ]
               ]

--------------------

e3 ∷ Entry
e3 = Entry { _recordNumber = ID 3
           , _title =
             Title $ T.unwords [ "The Amazing Spider-Man (1978)"
                               , "aka Spiderman" ]
           , _medium = 𝕵 TVSeries
           , _actresses = ["Madeleine Stowe"]
           , _description = Description $
               T.unlines [ T.unwords
                             [ "As a kidnapped foreign"
                             , "princess, she is kidnapped by"
                             , "terrorists. She."
                             ]
                         , T.unwords
                             [ "is sitting in a warehouse talking"
                             , "to one of her captors, and is" ]
                         , T.unwords
                             [ "gagged with white cloth between"
                             , "the teeth (on screen). Short"
                             , "scene,"
                             ]
                         , T.unwords
                             [ "but some pretty good closeups. She"
                             , "is wearing a purple sleeveless" ]
                         , T.unwords [ "gown." ]
                         ]
           , _tags = [ "bonddesc_anklestogether"
                     , "bonddesc_handsbehind", "gagtype_cleave"
                     , "onscreen_gagging", "onscreen_tying"
                     , "outfit_skirt", "restraint_rope"
                     , "country_us"]
           , _episode = 𝕵 (mkEpisode [1,6] (𝕵"\"Escort to Danger\""))
           , _entryDate = Day.epoch
           }

----------------------------------------

t131 ∷ 𝕋
t131 = T.unlines [ "Record number: 131"
                 , "Title: Starsky & Hutch aka Starsky and Hutch (1976)"
                 , "Medium: TV Series"
                 , "Actress: Linda Scruggs aka Linda Scruggs-Bogart"
                 , T.unwords [ "Description: Episode: \"The Specialist\" aka"
                             , "\"The Professionals\" (2.08)" ]
                 , T.unwords [ "In the climactic"
                             , "scene, new recruit Sally (Scruggs) is shown"
                             , "standing against a boiler, hands tied behind"
                             , "her back and against the boiler with white"
                             , "rope, and gagged with a strip of white tape."
                             , "Hutch discovers her, peels off the tape (with"
                             , "some effort), but opts to leave her there and"
                             , "go after the villain instead. She is not happy"
                             , "about this. Her capture is shown a while"
                             , "earlier, when she is grabbed and handgagged"
                             , "from a restaurant where she is posing as a"
                             , "waitress (all along wearing a very cute red"
                             , "minidress with fishnets). All the action occurs"
                             , "in the last 15mins of the episode." ]
                 , T.unwords [ "Tags: bonddesc_handsbehind, gagtype_hand,"
                             , "gagtype_tape, outfit_skirt, restraint_rope" ]
                 ]

--------------------

e131 ∷ Entry
e131 = Entry { _recordNumber = ID 000131
             , _title       = "Starsky & Hutch aka Starsky and Hutch (1976)"
             , _medium      = 𝕵 TVSeries
             , _actresses   = [ "Linda Scruggs aka Linda Scruggs-Bogart" ]
             , _tags        = [ "bonddesc_handsbehind", "gagtype_hand"
                              , "gagtype_tape", "outfit_skirt", "restraint_rope"
                              ]
             , _entryDate   = Day.epoch
             , _description = Description $
                 T.unwords [ "In the climactic scene, new recruit Sally"
                           , "(Scruggs) is shown standing against a boiler,"
                           , "hands tied behind her back and against the boiler"
                           , "with white rope, and gagged with a strip of white"
                           , "tape. Hutch discovers her, peels off the tape"
                           , "(with some effort), but opts to leave her there"
                           , "and go after the villain instead. She is not"
                           , "happy about this. Her capture is shown a while"
                           , "earlier, when she is grabbed and handgagged from"
                           , "a restaurant where she is posing as a waitress"
                           , "(all along wearing a very cute red minidress with"
                           , "fishnets). All the action occurs in the last"
                           , "15mins of the episode."
                           ]
             , _episode  =
                 𝕵 (mkEpisode [2,8]
                     (𝕵 "\"The Specialist\" aka \"The Professionals\""))
             }


----------------------------------------

t158 ∷ 𝕋
t158 = T.unlines [ "Record number: 158"
                 , "Title: Ninja III: The Domination (1984)"
                 , "Medium: Movie"
                 , "Actress: Lucinda Dickey"
                 , T.unwords [ "Description: About halfway through, she"
                             , "appears, ungagged, standing bound"
                             , "between two posts by ropes tied to"
                             , "leather cuffs around her outstretched"
                             , "wrists, and by two chains attached to a"
                             , "belt around her midsection, as she"
                             , "undergoes a ritual to call up the spirit"
                             , "of a ninja that has possessed her."
                             ]
                 ]

--------------------

e158 ∷ Entry
e158 = Entry { _recordNumber = ID 158
             , _title = "Ninja III: The Domination (1984)"
             , _medium = 𝕵 Movie
             , _actresses = ["Lucinda Dickey"]
             , _description = Description $
                 T.unwords [ "About halfway through, she appears, ungagged,"
                           , "standing bound between two posts by ropes"
                           , "tied to leather cuffs around her outstretched"
                           , "wrists, and by two chains attached to a belt"
                           , "around her midsection, as she undergoes a"
                           , "ritual to call up the spirit of a ninja that"
                           , "has possessed her."
                           ]
             , _tags = []
             , _episode = 𝕹
             , _entryDate = Day.epoch
             }

----------------------------------------

t12242 ∷ 𝕋
t12242 = T.unlines [ "Record number: 12242"
                   , "Title: CSI: Crime Scene Investigation aka C.S.I."
                   , "Medium: TV Series"
                   , "Actress: Kay Panabaker"
                   , "Description: Episode: \"Built to Kill\" Part 2 (7.2)"
                   , T.unwords [ "Lindsey (Panabaker), the daughter of"
                               , "series regular Catherine (Marg Helgenberger)"
                               , "is abducted during a car accident. She is"
                               , "later shown on a still image via a PC"
                               , "monitor, looking very helpless; she sits on"
                               , "a chair, hands duct taped to the armrests,"
                               , "feet unbound, gagged with a wide strip of"
                               , "duct tape, and blindfolded with more tape"
                               , "wrapped once around her head. The original"
                               , "polaroid of her like this is shown earlier,"
                               , "at about 33mins (based on a 1hr broadcast"
                               , "with commercials). Shortly afterwards the"
                               , "hideout is raided and she is released; very"
                               , "realistic onscreen ungagging and"
                               , "un-blindfolding, with the tape being slowly"
                               , "peeled off and pulling her skin back. It is"
                               , "clearly shown to the camera that the tape"
                               , "had no \"protection\" for her lips or eyes."
                               ]
                   , T.unwords [ "Tags: bonddesc_blindfold,",
                                 "bonddesc_handsspread, gagtype_tape,"
                               , "restraint_tape"]
                   ]

----------------------------------------

e12242 ∷ Entry
e12242 = Entry { _recordNumber = ID 012242
               , _title = "CSI: Crime Scene Investigation aka C.S.I."
               , _medium = 𝕵 TVSeries
               , _actresses = [ "Kay Panabaker" ]
               , _tags = [ "bonddesc_blindfold", "bonddesc_handsspread"
                         , "gagtype_tape", "restraint_tape" ]
               , _episode = 𝕵 (mkEpisode [7,2] (𝕵 "\"Built to Kill\" Part 2"))
               , _entryDate = Day.epoch
               , _description = Description $
                 T.unwords ["Lindsey (Panabaker), the daughter of series regular Catherine (Marg"
                   , "Helgenberger) is abducted during a car accident. She is later shown on a still"
                   , "image via a PC monitor, looking very helpless; she sits on a chair, hands duct"
                   , "taped to the armrests, feet unbound, gagged with a wide strip of duct tape,"
                   , "and blindfolded with more tape wrapped once around her head. The original"
                   , "polaroid of her like this is shown earlier, at about 33mins (based on a 1hr"
                   , "broadcast with commercials). Shortly afterwards the hideout is raided and she"
                   , "is released; very realistic onscreen ungagging and un-blindfolding, with the"
                   , "tape being slowly peeled off and pulling her skin back. It is clearly shown to"
                   , "the camera that the tape had no \"protection\" for her lips or eyes." ]
               }

----------------------------------------

ets ∷ [(𝕋,Entry)]
ets = [(t1,e1), (t3,e3), (t158,e158), (t131,e131), (t12242, e12242)]

-- that's all, folks! ----------------------------------------------------------
