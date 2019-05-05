module Projects exposing (Project, projects)


type alias Project =
    { name : String
    , link : String
    , contributorLevel : String
    , contact : String
    , description : String
    }


projects : List Project
projects =
    [ { name = "arithmoi"
      , link = "https://github.com/cartazio/arithmoi"
      , contributorLevel = "Beginner, Intermediate, Advanced"
      , contact = "Bodigrim"
      , description = "arithmoi is a Haskell library for number theory: modular arithmetic, primes, elliptic curves, etc. It is also an excellent tool for Project Euler."
      }
    , { name = "Asterius"
      , link = "https://github.com/tweag/asterius"
      , contributorLevel = "Advanced"
      , contact = "Shao Cheng"
      , description = "A Haskell to WebAssembly compiler."
      }
    , { name = "beans"
      , link = "https://github.com/sboehler/beans"
      , contributorLevel = "Beginner, Intermediate, Advanced"
      , contact = "Silvio Böhler"
      , description = "An accounting tool inspired by beancount and ledger."
      }
    , { name = "Cabal"
      , link = "https://haskell.org/cabal"
      , contributorLevel = "Intermediate, Advanced"
      , contact = "Mikhail Glushenkov"
      , description = "Cabal is a system for building and packaging Haskell libraries and programs."
      }
    , { name = "Darcs"
      , link = "https://darcs.net"
      , contributorLevel = "Intermediate, Advanced"
      , contact = "Ganesh Sittampalam"
      , description = "The darcs version control system."
      }
    , { name = "DataHaskell core"
      , link = "https://github.com/DataHaskell/dh-core"
      , contributorLevel = "Beginner, Intermediate, Advanced"
      , contact = "ocramz"
      , description = "Figuring out a native library and API for data science in Haskell."
      }
    , { name = "Dhall Kubernetes"
      , link = "https://github.com/dhall-lang/dhall-kubernetes"
      , contributorLevel = "Beginner"
      , contact = "Arian van Putten"
      , description = "Dhall k8s is a library for working with k8s in dhall. It currently generates dhall code from swagger spec using python. However I wanna port it to Haskell so I can use the Dhall AST directly instead of strings."
      }
    , { name = "Dronekit"
      , link = "https://github.com/dim1tri/dronekit-haskell"
      , contributorLevel = "Beginner, Intermediate"
      , contact = "dim1tri"
      , description = "Haskell API to communicate with vehicles over MAVLink."
      }
    , { name = "Dynamic Linking in GHC"
      , link = "https://ghc.haskell.org/trac/ghc/ticket/11238"
      , contributorLevel = "Intermediate"
      , contact = "Peter Trommler"
      , description = "Implement dynamic linking on Linux to match features of the RTS linker."
      }
    , { name = "Electronic Health Records Management as a Service"
      , link = "https://sites.google.com/view/electronichealthrecords/home"
      , contributorLevel = "Intermediate, Advanced"
      , contact = "Manu Gupta"
      , description = "Enabling the bottom of pyramid through EHR and Radiology services on the cloud using open source spreadsheet powered by Glasgow Haskell Compiler on AWS and Google Cloud, Open Source Picture Archiving and Communications System and PyDICOM, Ethereum Blockchain Network. EHR as a service aims at providing secure data storage, transparent data movement and data authenticity along with early detection and prevention of diseases."
      }
    , { name = "Elescore"
      , link = "https://elescore.de"
      , contributorLevel = "Beginner, Intermediate"
      , contact = "Akii"
      , description = "Elevator and escalator disruption analysis."
      }
    , { name = "Ex-Hack"
      , link = "https://github.com/NinjaTrappeur/ex-hack"
      , contributorLevel = "Beginner, Intermediate, Advanced"
      , contact = "Ninjatrappeur"
      , description = "Example-based Haskell Documentation"
      }
    , { name = "Extending and Adapting Octave using Glasgow Haskell Compiler and Open Source Spreadsheet"
      , link = "https://seeta.in"
      , contributorLevel = "Beginner, Intermediate"
      , contact = "Manu Gupta"
      , description = "We wish to extend and adapt octave (open source version of matlab) using Glasgow Haskell Compiler and Open Source spreadsheet namely Ethercalc. This will enable students and researchers to actively use Matlab for a variety of high performance and super computing applications in the fields of Physics, Chemistry, Biology and Mathematics."
      }
    , { name = "ff"
      , link = "https://github.com/ff-notes"
      , contributorLevel = "Intermediate"
      , contact = "Yuriy Syrovetskiy"
      , description = "Task tracker and note taker."
      }
    , { name = "Ginger"
      , link = "https://ginger.tobiasdammers.nl/"
      , contributorLevel = "Beginner, Intermediate, Advanced"
      , contact = "Tobias Dammers"
      , description = "A pure Haskell implementation of the Jinja2 HTML template language."
      }
    , { name = "Gonimo"
      , link = "https://gonimo.com"
      , contributorLevel = "Intermediate, Advanced"
      , contact = "Robert Klotzner"
      , description = "Web based baby monitor"
      }
    , { name = "GRIN"
      , link = "https://github.com/grin-tech/grin"
      , contributorLevel = "Intermediate, Advanced"
      , contact = "Peter Podlovics"
      , description = "GRIN is short for Graph Reduction Intermediate Notation, a modern back end for lazy functional languages. Most of the currently available compilers for such languages share a common flaw: they can only optimize programs on a per-module basis. The GRIN framework allows for interprocedural whole program analysis, enabling optimizing code transformations across functions and modules as well."
      }
    , { name = "Haskell Katas"
      , link = "https://haskellmad.github.io/haskellnautas/articulos/2018-05-31-haskell-con-katas-introduccion-a-codewars/"
      , contributorLevel = "Beginner"
      , contact = "Sofía Cordero"
      , description = "Learning methodology for Haskell"
      }
    , { name = "haskell-stdio"
      , link = "https://github.com/orgs/haskell-stdio"
      , contributorLevel = "Intermediate, Advanced"
      , contact = "winterland1989"
      , description = "IO toolkit based on libuv for GHC"
      }
    , { name = "hnix"
      , link = "https://github.com/haskell-nix/hnix"
      , contributorLevel = "Intermediate, Advanced"
      , contact = "John Wiegley"
      , description = "An implementation of the Nix language in Haskell"
      }
    , { name = "Integrated Water Management Resource Center"
      , link = "https://sites.google.com/view/integratedwatermanagment/home"
      , contributorLevel = "Intermediate, Advanced"
      , contact = "Manu Gupta"
      , description = "Enabling the Bottom of Pyramid through Water Management Services on the Cloud build using Reflex FRP framework, Water Quality Tools, Open Source Spreadsheet with Haskell server stack and Picture Archiving and Communication System. Remote Water Monitoring, Sewage Management, Quality Assurance and Recyclability of Water using a software stack build with Glasgow Haskell Compiler."
      }
    , { name = "Juvix"
      , link = "https://github.com/cwgoes/juvix"
      , contributorLevel = "Advanced"
      , contact = "Christopher Goes"
      , description = "Haskell/Idris to Michelson/WASM smart contract compiler (work in progress)."
      }
    , { name = "massiv"
      , link = "https://github.com/lehins/massiv"
      , contributorLevel = "Beginner, Intermediate, Advanced"
      , contact = "Alexey Kuleshevich"
      , description = "Efficient Haskell Arrays featuring Parallel computation."
      }
    , { name = "Shake"
      , link = "https://shakebuild.com"
      , contributorLevel = "Intermediate, Advanced"
      , contact = "Neil Mitchell"
      , description = "Shake build system."
      }
    , { name = "nix-tools"
      , link = "https://github.com/angerman/nix-tools"
      , contributorLevel = "Beginner, Intermediate, Advanced"
      , contact = "angerman"
      , description = "A better haskell infrastructure for nix"
      }
    , { name = "(no name)"
      , link = ""
      , contributorLevel = "Intermediate"
      , contact = "ludat"
      , description = "I'd like to build a haskell config library along the lines of Java's spring-config."
      }
    , { name = "Ormolu"
      , link = ""
      , contributorLevel = "Intermediate, Advanced"
      , contact = "Mark Karpov"
      , description = "A new formatter for Haskell source code."
      }
    , { name = "pandoc"
      , link = "http://pandoc.org"
      , contributorLevel = "Beginner, Intermediate, Advanced"
      , contact = "Mauro Bieg"
      , description = "Universal document converter"
      }
    , { name = "Pomelo"
      , link = "https://temporal-pomelo.com/"
      , contributorLevel = "Advanced"
      , contact = "Wolfgang Jeltsch"
      , description = "Pomelo is a new FRP library, whose development is just starting. Key goals for Pomelo are a clean semantics and an efficient implementation."
      }
    , { name = "PPP"
      , link = "http://PPP.pfff"
      , contributorLevel = "Beginner"
      , contact = "Tiphaine LAURENT"
      , description = "Piffomètre Programming Paradigm."
      }
    , { name = "Reflex Site Builder"
      , link = ""
      , contributorLevel = "Intermediate"
      , contact = "Ru Horlick"
      , description = "A UI based website builder using Reflex and GHCJS. Down with WordPress!"
      }
    , { name = "Rome"
      , link = "https://github.com/blender/Rome"
      , contributorLevel = "Intermediate"
      , contact = "Tommaso Piazza"
      , description = "Remote cache for binary artifacts."
      }
    , { name = "servant"
      , link = "https://haskell-servant.github.io/"
      , contributorLevel = "Beginner, Intermediate, Advanced"
      , contact = "Alp"
      , description = "A library for writing web applications, deriving clients and more."
      }
    , { name = "Smos"
      , link = "https://smos.cs-syd.eu"
      , contributorLevel = "Beginner, Intermediate"
      , contact = "Mr Kerckhove"
      , description = "A purely functional semantic forest editor to replace Emacs&#39; Org Mode."
      }
    , { name = "snack"
      , link = "https://github.com/nmattia/snack/"
      , contributorLevel = "Advanced"
      , contact = "Nicolas Mattia"
      , description = "Nix-based incremental build tool for Haskell projects."
      }
    , { name = "spago"
      , link = "https://github.com/spacchetti/spago"
      , contributorLevel = "Beginner, Intermediate"
      , contact = "Fabrizio Ferrai"
      , description = "PureScript package manager and build tool powered by Dhall and Spacchetti."
      }
    , { name = "sudbury"
      , link = "https://github.com/abooij/sudbury"
      , contributorLevel = "Intermediate, Advanced"
      , contact = "Auke Booij"
      , description = "Haskell implementation of the wayland protocol."
      }
    , { name = "TaskLite"
      , link = "https://github.com/ad-si/tasklite"
      , contributorLevel = "Intermediate"
      , contact = "Adrian Sieber"
      , description = "CLI task / todo manager built with Haskell and SQLite"
      }
    , { name = "The GRIN Compiler"
      , link = "https://github.com/grin-tech/grin"
      , contributorLevel = "Advanced"
      , contact = "Csaba Hruska"
      , description = "GRIN is a compiler back-end for lazy and strict functional languages with whole program optimization support."
      }
    , { name = "traze"
      , link = "https://github.com/iteratec/traze-gameserver"
      , contributorLevel = "Beginner, Intermediate, Advanced"
      , contact = "Ben Brunzel"
      , description = "traze is a MQTT based tronlike multiplayer game with a Gameserver implementation in Haskell."
      }
    ]
