module Main exposing (..)

import Html
import Html.Attributes
import Html.Events
import Markdown


---- MODEL ----


type alias Model =
    { content : List String
    , tool : Tool
    , selectedParagraph : Int
    }


type Tool
    = Title
    | Quote
    | Edit


init : ( Model, Cmd Msg )
init =
    ( { content = content
      , tool = Title
      , selectedParagraph = -1
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = ParagraphClicked Int
    | SelectTool Tool
    | EditParagraph Int String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ParagraphClicked index ->
            let
                newContent =
                    model.content
                        |> List.indexedMap
                            (\i paragraph ->
                                if i == index then
                                    applyTool model.tool paragraph
                                else
                                    paragraph
                            )
            in
                ( { model | content = newContent, selectedParagraph = index }, Cmd.none )

        SelectTool tool ->
            ( { model | tool = tool }, Cmd.none )

        EditParagraph index text ->
            ( { model
                | content =
                    List.indexedMap
                        (\i paragraph ->
                            if i == index then
                                text
                            else
                                paragraph
                        )
                        model.content
              }
            , Cmd.none
            )


applyTool : Tool -> String -> String
applyTool tool paragraph =
    case tool of
        Title ->
            if String.startsWith "## " paragraph then
                String.dropLeft 3 paragraph
            else if String.startsWith "# " paragraph then
                "#" ++ paragraph
            else
                "# " ++ paragraph

        Quote ->
            let
                applyTransform : (String -> String) -> String
                applyTransform transform =
                    String.split "\n" paragraph
                        |> List.map transform
                        |> String.join "\n"
            in
                if String.startsWith "> " paragraph then
                    applyTransform (String.dropLeft 2)
                else
                    applyTransform ((++) "> ")

        Edit ->
            -- There's nothing to transform here, we're displaying textareas and reacting onInput.
            paragraph



---- VIEW ----


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ viewToolbar model.tool
        , Html.div [ Html.Attributes.class "main" ]
            [ Html.h1 [] [ Html.text "Markdown Formatter" ]
            , List.indexedMap
                -- viewParagraph takes two more arguments: index and the
                -- paragraph, that are passed by List.indexedMap
                (viewParagraph model.tool model.selectedParagraph)
                model.content
                |> Html.div [ Html.Attributes.class "wrapper" ]
            ]
        ]


viewToolbar : Tool -> Html.Html Msg
viewToolbar selectedTool =
    let
        radio : Tool -> Html.Html Msg
        radio tool =
            Html.label []
                [ Html.input
                    [ Html.Attributes.type_ "radio"
                    , Html.Attributes.name "toolbar"
                    , Html.Attributes.checked (tool == selectedTool)
                    , Html.Events.onClick (SelectTool tool)
                    ]
                    []
                , Html.text (toString tool)
                ]
    in
        Html.div [ Html.Attributes.class "toolbar" ]
            [ radio Title
            , radio Quote
            , radio Edit
            ]


viewParagraph : Tool -> Int -> Int -> String -> Html.Html Msg
viewParagraph tool selectedParagraph index paragraph =
    let
        classNames =
            if tool == Edit && selectedParagraph == index then
                "paragraph edit"
            else
                "paragraph display"
    in
        Html.div
            [ Html.Attributes.class classNames
            ]
            [ Html.textarea
                [ Html.Events.onInput (EditParagraph index)
                ]
                [ Html.text paragraph ]
            , Markdown.toHtml
                [ Html.Attributes.class "markdown"
                , Html.Events.onClick (ParagraphClicked index)
                ]
                paragraph
            ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }


content : List String
content =
    String.split "\n\n"
        """# Chapitre 1 : Le secret du mariage

> L'homme quittera son père et sa mère pour s'attacher à sa femme
> et les deux ne seront plus qu'une seule chair. Il y a là un grand mystère ...
>
> Éphésiens 5.31-32

J'en ai assez d'entendre des discours sentimentaux sur le mariage. Que ce soit lors de cérémonies nuptiales, à l'église ou à l'école du dimanche, une bonne part de ce qui est dit a autant de profondeur que ce qu'on lit sur les cartes de vœux. Le mariage est tout *sauf* sentimental. Il est splendide, mais difficile. Il est fait de joie vive et de force, mais aussi de sang, de sueur et de larmes; de défaites humiliantes et de victoires épuisantes. Après quelques semaines d'existence, aucun mariage ne ressemble à un conte de fées. Rien de surprenant donc, à ce que la seule phrase à laquelle de nombreux couples s'identifient, dans le célèbre discours de Paul sur le mariage en Éphésiens 5, soit le verset 32, cité plus haut. Parfois, vous vous effondrez dans votre lit, après une longue et dure journée où vous avez essayé de vous comprendre l'un l'autre, et vous ne parvenez qu'à soupirer: « Il y a bien là un grand mystère! » À d'autres moments, votre mariage ressemble à un puzzle inachevé, un labyrinthe dans lequel vous vous sentez perdu.

Je pense que tout cela est vrai. Et pourtant, il n'existe aucune relation entre deux êtres humains, aussi belle et importante que le mariage. Dans le récit biblique, Dieu lui-même célèbre la première union (Genèse 2.22-25). À la vue de sa femme, l'homme devient poète et s'écrie: « Enfin[^8] ! » Tout dans le texte révèle que le mariage, après notre relation avec Dieu, est la relation la plus profonde qui soit. C'est pourquoi, tout comme apprendre à connaître Dieu personnellement, parvenir à connaître et à aimer son conjoint est difficile et douloureux, mais également gratifiant et merveilleux.

L'expérience la plus douloureuse et la plus merveilleuse concerne la compréhension biblique du mariage. Aucune autre époque n'a eu un tel besoin d'élever cette vision et de lui donner la première place dans notre culture.

## Le déclin du mariage

Depuis 40 ans, les « principaux indicateurs du mariage » sont en déclin constant[^9]. Ils nous alertent de façon empirique sur la santé du mariage et son degré de satisfaction aux États-Unis. Le taux de divorce a presque doublé ces 50 dernières années[^10]. En 1970, 89 % des enfants naissaient de parents mariés, aujourd'hui, ils ne sont plus que 60 %[^11]. Encore plus révélateur, en 1960, 72 % des Américains adultes étaient mariés; en 2008, ils ne sont plus que 50 %[^12].

Tout ceci dénote une méfiance et un pessimisme croissants dans notre culture au sujet du mariage, et c'est particulièrement vrai chez les jeunes adultes. Ils estiment avoir peu de chance de vivre un mariage heureux et, même dans l'éventualité d'une union stable, la perspective d'une vie sexuelle ennuyeuse les horrifie. Le comédien Chris Rock résume: « Vous avez le choix: mariage et monotonie, ou célibat et solitude! » Pour de nombreux jeunes adultes, ce sont là les deux options principales. C'est la raison pour laquelle ils envisagent le concubinage, solution intermédiaire entre le mariage et de simples rencontres sexuelles.

Ces trois dernières décennies, cette pratique a connu une croissance exponentielle. Aujourd'hui, plus de la moitié des gens vivent ensemble avant de se marier, contrairement aux années 60 où l'union libre restait une exception[^13]. Un quart des femmes célibataires entre 25 et 39 ans, vivent avec un partenaire et plus de 60 % à l'approche de la quarantaine[^14]. Cette pratique est induite par plusieurs croyances populaires. L'une d'elles suppose que la plupart des mariages sont malheureux. Après tout, raisonne-t-on, 50 % des mariages finissent par un divorce et 50 % doivent être en majorité épouvantables. Vivre ensemble avant de se marier, argumente-t-on, c'est mettre plus de chances de son côté pour faire le bon choix. Cela permet de vérifier si l'on est compatible avant de faire le grand saut. Il s'agit d'un moyen de découvrir si l'autre est vraiment digne de notre intérêt, si « la bonne entente sexuelle » est suffisamment forte. « Toutes les personnes que je connais et qui se sont mariées rapidement, sans d'abord vivre ensemble, ont fini par divorcer » a répondu un homme lors d'un sondage de l'institut Gallup[^15].

Malheureusement, le problème de ces croyances et suppositions, c'est qu'elles sont toutes pratiquement fausses.

## Les vertus surprenantes du mariage

Malgré la déclaration du jeune homme lors du sondage Gallup, il s'avère qu'« un nombre conséquent de statistiques indique que ceux qui vivent ensemble avant le mariage sont plus susceptibles de se séparer ensuite[^16]. » Le choix du concubinage, par ceux qui ont vécu la douleur du divorce de leurs parents, est une réaction compréhensible; mais dans les faits, il semblerait que le remède soit pire que le prétendu mal[^17].

D'autres croyances populaires sont également erronées. Environ 45 % des mariages se terminent effectivement par un divorce, mais le pourcentage de loin le plus élevé se trouve chez ceux qui se marient avant 18 ans, quittent le lycée et ont eu un bébé avant de se marier. « Par conséquent, si vous avez un niveau d'instruction satisfaisant et un revenu décent, si vous venez d'une famille intacte, si vous êtes croyant et si vous vous mariez après l'âge de 25 ans sans avoir eu d'enfant avant, vos probabilités de divorce sont vraiment faibles[^18]. »

De nombreux jeunes adultes défendent le concubinage pour des raisons matérielles. Il leur paraît primordial d'accéder à la propriété et d'être stables financièrement avant de se marier[^19]. Ils s'imaginent que le mariage est un gouffre financier. Mais les études démontrent « les surprenants avantages économiques du mariage[^20] ». Une étude menée en 1992 sur des retraités montre que ceux qui étaient toujours mariés avaient 75 % de ressources de plus que ceux qui ne s'étaient jamais mariés, ou qui avaient divorcé et ne s'étaient pas remariés. Encore plus remarquable, cette étude établit que les hommes mariés gagnaient 10 à 40 % de plus que les célibataires, tout en ayant un niveau d'éducation et d'expérience professionnelle similaires.

Comment l'expliquer? En partie par la meilleure santé physique et mentale dont jouissent les couples mariés. De plus, le mariage est un puissant « amortisseur de chocs » qui aide à traverser les déceptions, la maladie et d'autres difficultés. L'équilibre est plus vite retrouvé. Les revenus plus élevés s'expliquent probablement aussi par ce que les spécialistes appellent les « normes sociales conjugales ». Des études montrent que les conjoints s'encouragent l'un l'autre à plus de responsabilité et de maîtrise de soi. Les amis ou les membres de la famille n'auraient pas la même influence. À titre d'exemple, des célibataires peuvent se laisser aller à de folles dépenses sans rendre de comptes à personne. Par contre, les couples s'entraînent mutuellement à l'épargne, à l'investissement et au plaisir différé. Il n'y a rien de mieux que le mariage pour amener quelqu'un à la maturité.

Ce qui rend les jeunes adultes si méfiants vis-à-vis du mariage est probablement la croyance que la plupart des couples mariés sont malheureux. Un exemple typique figure dans un forum de Yahoo!, où un jeune homme de 24 ans annonce sa décision de ne jamais se marier. Il rapporte qu'au cours des derniers mois, il en a fait part à ses amis mariés. Tous ont ri, l'ont jalousé et trouvé intelligent. Il en a conclu qu'au moins 70 % des gens mariés devaient être malheureux. En réponse à ce message, une jeune femme répond que cela confirme ses propres observations. Elle estime: « Sur 10 couples mariés [...] 7 sont mortellement malheureux ». Elle ajoute: « Je me marie l'année prochaine parce que j'aime mon fiancé. Mais si les choses changent, je n'hésiterai pas à divorcer[^21]. »

Récemment, le *New York Times* Magazine a publié un article sur un nouveau film, *Monogamy*, réalisé par Dana Adam Shapiro[^22] qui s'était rendu compte en 2008 que plusieurs de ses amis, trentenaires et mariés, s'étaient séparés. Avec l'idée de faire un film basé sur une histoire orale de ruptures, il a interrogé en profondeur 50 personnes qui avaient vécu un divorce. Il n'a fait cependant aucune recherche sur les mariages de longue durée et heureux. Quand on lui en a demandé la raison, il a paraphrasé Tolstoï:« Tous les couples heureux sont identiques, c'est-à-dire qu'ils sont tout simplement ennuyeux[^23]. » « Il n'est donc pas surprenant, conclut le journaliste du *Times*, de dire que le film adopte finalement une vision glauque, voire complètement apocalyptique, des relations. » Le film dépeint deux personnes qui s'aiment beaucoup, mais pour qui « ça ne marche pas ». Dans d'autres interviews, le réalisateur déclare que deux individus modernes éprouveront une extrême difficulté à s'aimer sans étouffer l'individualité et la liberté de l'autre. D'après le journaliste, Shapiro, célibataire endurci bien qu'espérant se marier un jour, n'a pas conçu ce film dans un esprit « anti-mariage », mais il éprouve toutefois une grande réticence envers la monogamie. Il reflète ainsi l'opinion typique des jeunes adultes, particulièrement ceux des zones les plus urbaines des États-Unis.

Pasteur d'une Église comptant plusieurs milliers de célibataires à Manhattan, j'ai discuté avec un grand nombre d'hommes et de femmes qui partageaient la même opinion négative du mariage. Pourtant ils sous-estiment les perspectives d'un mariage réussi. Tous les sondages indiquent que le pourcentage de personnes qui se disent « très heureuses » dans leur mariage est élevé (environ 61-62 %). Ce score a très peu diminué cette dernière décennie. Plus étonnant encore, les études transversales montrent que les deux tiers des mariages malheureux deviennent heureux dans les 5 ans qui suivent, si le couple choisit de ne pas divorcer[^24]. Cette découverte a fait dire à Linda J. Waite, sociologue à l'Université de Chicago: « Les mérites du divorce ont été exagérés[^25] ».

Les résultats de la grande majorité des recherches effectuées ces 20 dernières années indiquent que ceux qui sont mariés et qui le restent sont bien plus satisfaits de leur vie que les célibataires, les divorcés ou ceux qui vivent en concubinage[^26]. Ils révèlent également que la plupart des gens sont heureux dans leur mariage et que ceux qui ne le sont pas et ne divorcent pas, finissent généralement par l'être. De plus, les enfants issus de familles où les parents sont mariés, ont une qualité de vie deux à trois fois supérieure aux autres[^27]. Le verdict est écrasant: être marié et grandir avec des parents mariés, sont de puissants stimuli pour le bien-être humain.

## L'histoire du mariage

Croire à l'attrait et à la vertu du mariage était, jadis, une tendance universelle. Ce n'est plus le cas à notre époque. Un récent rapport de l'Université de Virginie (le National Marriage Project) conclut ; « Moins d'un tiers des étudiantes (en année de bac) et à peine plus d'un tiers des étudiants semblent persuadés [...] que le mariage offre davantage de bénéfices que les autres options. Pourtant, cette attitude négative va à l'encontre des données empiriques disponibles qui montrent les avantages importants, autant personnels que sociaux, du mariage par rapport au célibat ou à l'union libre[^28]. » Toujours d'après cette étude, le point de vue de la majorité des jeunes adultes va à l'encontre de l'avis général des personnes plus âgées et des enseignements des principales religions du monde. Il est également infirmé par la recherche la plus récente en sciences sociales.

D'où vient donc ce pessimisme, et pourquoi est-il si déconnecté de la réalité? Paradoxalement, il se peut qu'il soit le résultat d'une nouvelle idéalisation irréaliste du mariage, due à un changement significatif de son but dans notre culture. L'éminent juriste John Witte Jr. estime que l'ancien « idéal du mariage en tant qu'union contractuelle permanente conçue dans l'intérêt de l'amour réciproque, de la procréation et de la protection, cède lentement la place à une nouvelle réalité. Il s'agit désormais d'un "contrat à portée sexuelle", conçu pour assurer la satisfaction individuelle de chaque partie[^29]. »

Witte fait remarquer que, dans les civilisations occidentales, plusieurs courants de pensée ont rivalisé quant à « la forme et la fonction » du mariage[^30]. Les deux premiers, catholique et protestant, bien qu'ils diffèrent sur plusieurs points, ont enseigné que le but du mariage était de créer une structure d'engagement et d'amour pour la vie, entre mari et femme. Il s'agissait d'un lien solennel conçu pour aider chacune des parties à maîtriser ses propres pulsions et intérêts, au profit de la relation, pour que cela soit un sacrement de l'amour de Dieu (vision catholique) et serve le bien commun (vision protestante). Le mariage était donné par Dieu non seulement pour les chrétiens, mais aussi pour le bien-être de toute l'humanité. Il forgeait le caractère, réunissant l'homme et la femme en un vrai partenariat. Telle en était la compréhension des protestants. Ils considéraient surtout que le mariage pour la vie constituait le seul modèle de stabilité sociale où les enfants puissent grandir et s'épanouir. La société tirait donc un avantage direct de l'institution du mariage puisqu'aucun autre cadre ne pouvait offrir aux enfants un tel épanouissement[^31].

Cependant, explique Witte, une nouvelle conception du mariage est née au XVIIIe siècle, celui des Lumières, et au XIXe siècle. Les cultures anciennes enseignaient le sens du devoir, la place dévolue à chacun dans la société et le dévouement. Les Lumières ont changé les choses et donné un nouvel éclairage: ce qui donne son sens à la vie, c'est la liberté pour l'individu de choisir la vie qui lui apportera le maximum de satisfaction. La recherche d'épanouissement émotionnel et sexuel, et de réalisation personnelle a redéfini les contours du mariage. Les notions d'abnégation, de renoncement aux libertés personnelles, d'attachement aux obligations du mariage et de la famille ont perdu de leur saveur.

Les partisans de cette approche n'ont pas considéré l'essence du mariage, ni dans son symbolisme en tant que sacrement divin (la perspective catholique), ni en tant que lien social accordé pour le bien de l'humanité au sens large (la perspective protestante). Le mariage est plutôt devenu un contrat entre deux parties pour une croissance et une satisfaction personnelles mutuelles. De ce point de vue, les gens se sont mariés pour eux-mêmes et non pour remplir leurs obligations envers Dieu ou la société. Il fallait donc permettre aux parties de gérer le mariage de la manière dont elles allaient le juger bénéfique pour elles, sans qu'aucune obligation envers l'Eglise, la tradition ou la société ne leur soit imposée. En résumé, l'époque des Lumières a privatisé le mariage en le retirant de la sphère publique et a redéfini son objectif en termes de satisfaction personnelle et non en termes de « bien-être collectif », tels que refléter la nature de Dieu, développer le caractère ou éduquer des enfants. Cette nouvelle vision de la finalité du mariage a lentement mais sûrement fini par remplacer les autres visions plus anciennes de la culture occidentale.

Ce changement a été bien timide. Récemment, Tara Parker-Pope, chroniqueur au New York Times, a écrit un article intitulé « Le mariage heureux est le "mariaJE" » :

> L'idée selon laquelle les meilleurs mariages sont ceux qui satisfont l'individu peut sembler paradoxale. Après tout, le principe du mariage n'est-il pas de privilégier la relation? Plus maintenant. Pendant des siècles, le mariage a été perçu comme une institution économique et sociale, dont la survie même l'emportait sur les besoins intellectuels et émotionnels des conjoints. Dans les relations modernes, en revanche, les gens recherchent une vraie relation de partenariat avec un conjoint qui rende leur vie plus intéressante [ ... et où] ils s'entraident à atteindre des objectifs qui leur sont chers[^32]. »

Cette transformation a été révolutionnaire et Mme ParkerPope le déclare ouvertement. Le mariage, jadis une institution publique pour le bien commun, est aujourd'hui un arrangement privé destiné à la satisfaction de chaque conjoint. Auparavant, le mariage tournait autour de *nous*, mais aujourd'hui il tourne autour du *je*.

Ironie du sort, cette nouvelle vision fait peser, au bout du compte, un fardeau d'attentes bien plus écrasant, sur le mariage et sur les époux, que ne l'ont fait les interprétations traditionnelles précédentes. Nous nous retrouvons désespérément piégés entre des aspirations irréalistes et d'énormes craintes vis-à-vis du mariage.

## La quête d'une « âme sœur »

Une étude importante datant de 2002, intitulée« Pourquoi les hommes ne prennent aucun engagement » en est une claire illustration[^33]. Les femmes accusent souvent les hommes d'avoir « une phobie de l'engagement » et d'avoir peur de se marier. Les auteurs de ce rapport révèlent qu'en effet leur « enquête sur les attitudes masculines prouve le bien-fondé de cette perception populaire ». Ils énumèrent ensuite les raisons pour lesquelles les hommes déclarent préférer ne pas se marier, ou du moins pas tout de suite. Le fait le plus frappant, cependant, concerne le nombre élevé de ceux qui disent vouloir attendre de trouver « l'âme sœur parfaite », quelqu'un de très « compatible ». Que signifient ces propos?

Quand j'ai rencontré Kathy, ma future épouse, nous nous sommes très vite rendu compte que nous avions les mêmes goûts concernant une quantité inouïe de livres, d'histoires, de sujets de discussion. Nous avions la même façon d'envisager la vie et nous partagions les mêmes sources de joie. La possibilité d'une profonde amitié se profilait à l'horizon, chacun reconnaissant dans l'autre une véritable « âme sœur ». Mais de nombreux jeunes adultes ont une compréhension différente de ce terme. Selon Whitehead et Popenoe, deux facteurs clés entrent enjeu.

Le premier est l'attraction physique et l'alchimie sexuelle. L'un des thèmes prépondérants lors des entretiens de Shapiro avec des personnes récemment divorcées était l'importance donnée à une vie sexuelle épanouie. Une femme a expliqué qu'elle avait épousé son mari parce qu'elle le trouvait « sexy ». Mais, à son grand désarroi, il a pris du poids et a cessé de prendre soin de son apparence physique. La lune de miel était terminée. Elle en a surtout pris conscience lors de leurs rapports sexuels. Elle avait posé comme principe de ne pas faire l'amour à moins d'en avoir vraiment *envie*, mais cela ne lui arrivait pas souvent: « Nous étions tombés dans une routine dans laquelle nous avions des rapports seulement une fois par semaine, et parfois moins. Il n'y avait aucune variété, aucun bénéfice mental ou émotionnel. L'urgence ou la tension qui rend l'acte si magnifique avait disparu, tout comme ce désir de vouloir charmer ou séduire quelqu'un[^34] ... »

Elle estimait que l'attraction sexuelle et l'alchimie étaient des pré-requis fondamentaux pour trouver quelqu'un de compatible.

Cependant, dans l'étude précitée, les hommes sondés n'ont pas mentionné l'attirance sexuelle comme facteur principal. Selon eux, la notion de « compatibilité » s'appliquait avant tout à une femme « prête à les prendre tels qu'ils sont, sans vouloir les changer[^35] ». « Certains exprimaient leur rancœur envers les femmes qui avaient essayé de les changer. [...] D'autres décrivaient ainsi l'affinité conjugale: trouver une femme qui puisse "s'adapter à leur vie". "Si on est réellement compatible, on n'a pas besoin de changer", a affirmé l'un d'entre eux[^36]. »

## Retrouver la vraie masculinité

La rupture avec le passé est considérable. Autrefois, les hommes s'attendaient à ce que le mariage leur apporte de grands changements personnels. Un des aspects de la vision traditionnelle du mariage était son effet « civilisateur » sur les hommes. Ceux-ci étaient réputés plus indépendants, moins désireux et moins aptes que les femmes à entrer dans des relations nécessitant communication, soutien réciproque et partenariat. Un des objectifs classiques du mariage était donc très clairement de « changer» les hommes et d'être une •<école» dans laquelle ils apprendraient à développer de nouvelles relations, plus interdépendantes.

Comble d'ironie, les hommes de cette étude ont précisément fait ressortir les attitudes que le mariage était jadis censé corriger. Les chercheurs leur ont demandé s'ils étaient conscients que les femmes de leur âge ressentaient le besoin de se marier et d'avoir des enfants avant qu'elles ne le puissent plus sur le plan biologique. Ils savaient pertinemment qu'en différant le mariage, elles atteindraient plus difficilement cet objectif de vie, mais cela leur importait peu. Comme l'a dit l'un d'entre eux: « C'est leur problème[^37] ». Plusieurs participants sont restés inflexibles: leur relation avec une femme ne devait en aucun cas entraver leur liberté. Le rapport conclut: « L'union libre permet aux hommes d'avoir un accès permanent aux services domestiques et sexuels d'une petite amie, tout en [...] restant plus indépendants et en continuant leur recherche d'une meilleure partenaires[^38] ».

Dans un article du *New York Times*, Sara Lipton a dressé une liste d'hommes politiques célèbres qui avaient refusé de limiter leurs relations sexuelles à leur épouse: Arnold
Schwarzenegger, Dominique Strauss-Kahn, Bill Clinton, Mark Sanford, John Ensign, etc. Chacun s'était opposé à l'optique traditionnelle du mariage de changer ses instincts naturels, maîtriser ses passions, apprendre à sacrifier ses propres désirs et servir les autres.

L'explication populaire de ce comportement est que le mariage ne s'accorde tout simplement pas à la nature masculine; les hommes les plus virils ne sont pas faits pour le mariage. On soutient qu' « un besoin de conquête sexuelle, d'adulation des femmes et de liaisons risquées et illicites va de pair avec l'ambition, la volonté de réussir et la confiance en soi du "mâle dominant". » Pourtant, Lipton affirme que le mariage était traditionnellement le lieu où les mâles devenaient vraiment masculins: « Dans l'histoire du monde occidental, la caractéristique principale et la plus estimée de la masculinité était la plupart du temps la maîtrise de soi. [...] Un homme qui cédait à la gloutonnerie, à l'ivrognerie, à la paresse ou au libertinage, incapable de "se dominer", était considéré comme inapte à diriger sa famille, et à plus forte raison un gouvernement [...]».

Lipton conclut: « Au vu des révélations récentes sur le comportement sexuel inconsidéré et complaisant de tant de nos élus, il vaudrait peut-être la peine de se remémorer que la grandeur d'un homme se mesurait jadis à sa retenue et non à ses prouesses sexuelles[^39]. »

Il serait erroné de faire porter aux hommes toute la responsabilité de ce changement d'attitude à l'égard du mariage. Aujourd'hui, autant les hommes que les femmes désirent un mariage dans lequel ils se sentent émotionnellement et sexuellement satisfaits. Ils veulent que le conjoint les laisse « être eux-mêmes», qu'il soit drôle, intellectuellement stimulant, sexuellement attirant, avec beaucoup d'intérêts communs et qui, cerise sur le gâteau, les soutienne dans leurs objectifs personnels et dans leur style de vie actuel.

Si vous recherchez un partenaire qui n'exige guère de changements de votre part, vous souhaiterez également qu'il travaille presque toujours dans l'harmonie. Votre quête vous mènera vers quelqu'un « qui fonctionne bien », sans problèmes personnels dans ses bagages, et sans exigences. Vous cherchez, en somme, une personne idéale: heureuse, en bonne santé, intéressante et satisfaite de sa vie. Jamais, dans l'histoire de l'humanité, une société n'a été à ce point aussi idéaliste en ce qui concerne la recherche d'un conjoint.

## L'ironie de l'idéalisme pessimiste

Il semble presque contradictoire d'avancer que ce nouvel idéalisme est la conséquence d'un nouveau pessimisme concernant le mariage, pourtant c'est exactement ce qui s'est passé. Dans les générations précédentes, on parlait beaucoup moins de « compatibilité » et de trouver l'âme sœur idéale. Aujourd'hui, nous recherchons quelqu'un qui nous accepte tels que nous sommes et qui comble nos désirs, ce qui crée un ensemble d'attentes irréalistes frustrantes pour tous.

La recherche d'un partenaire sexuel convenable est un problème en soi. Dans un autre rapport du National Marriage Project on lit:

> La médiatisation de la culture pornographique risque [également] d'ancrer des attentes irréalistes quant à l'apparence de la future âme sœur. Influencés par les images de jeunes femmes sexy sur MTV, sur Internet, et sur les podiums des défilés télévisés [...], des hommes peuvent repousser l'idée d'épouser leur petite amie du moment dans l'espoir de finir par trouver un mélange « âme sœur/top model[^40] ».

Il serait néanmoins faux d'imputer le changement d'attitude de notre culture envers le mariage à la seule quête de beauté physique de la part des hommes. Les femmes sont tout aussi affectées par notre culture de consommation. Aujourd'hui, hommes et femmes ne perçoivent pas le mariage comme un moyen de développer le caractère et de faire évoluer la communauté, mais comme un moyen d'atteindre leurs objectifs de vie personnels. Ils recherchent tous un conjoint qui « comble leurs désirs émotionnels, sexuels et spirituels[^41] ». Cela crée un idéalisme extrême qui plonge dans un profond pessimisme : la rencontre avec la bonne personne à épouser devient peu probable. Voilà pourquoi tant de gens repoussent le mariage et se détournent d'excellents conjoints potentiels, simplement parce qu'ils ne sont pas « assez bien » à leurs yeux.

Quelle ironie! La conception ancienne du mariage est qualifiée de traditionaliste et d'oppressive, alors que la nouvelle notion de « mariaJE » semble si libératrice. Or c'est cette dernière qui a engendré un déclin rapide du mariage ainsi qu'un sentiment étouffant de désespoir à son évocation. Pour former un mariaJE, il faut deux individus parfaitement équilibrés et heureux, possédant très peu de besoins émotionnels ou avec seulement des petits défauts à corriger. Trouver un tel oiseau rare relève presque de l'impossible! La nouvelle conception du « mariage-développement de soi » nous a placés en position d'attentes maximales et minimales en même temps.

Dans son article humoristique « Pinailleur, pinailleur, pinailleur», John Tierney essaye de façon superbe de nous faire rire de situations impossibles propres à notre culture. Il restitue les raisons invoquées par ses amis célibataires pour ne pas avoir donné suite à leur récente relation:

> « Elle a mal prononcé "Goethe".»
>
> « Comment le prendre au sérieux après avoir vu Les relations amoureuses pour les nuls sur son étagère? » « Il suffisait qu'elle perde trois kilos. »
>
> « Oui, bien sûr, c'est un associé, mais pas d'une grande entreprise. En plus, il porte des socquettes noires.»
>
> « En fait, ça a bien commencé: [ ... ] beau visage, corps magnifique, joli sourire. Tout allait pour le mieux, jusqu'à ce qu'elle se retourne. » Sur un silence qui ne présage rien de bon, il hoche la tête. « [...] Ses coudes étaient sales[^42]. »

Après avoir scruté les petites annonces de rencontres, incroyablement irréalistes (où le partenaire recherché n'existe pratiquement jamais), John Tierney a estimé que les jeunes adultes souffraient terriblement de ce qu'il a appelé le « Détecteur de défauts ». Il s'agit d'une « voix intérieure, semblable à un petit appareil vrombissant dans le cerveau, qui détecte instantanément un défaut fatal chez tout partenaire potentiel ». Quelle est l'utilité du Détecteur de défauts? En voici l'un des usages : il serait mis au point par des personnes « déterminées à obtenir plus qu'elles ne méritent, et [à] rejeter toute personne qui leur ressemble, même de loin ». Cependant, conclut Tierney, ce détecteur sert d'excuse pour rester seul et, par conséquent, en sécurité. « Au fond d'eux-mêmes, ils savent pourquoi ils en ont besoin [...] Ce n'est pas facile à admettre, surtout le jour de la St-Valentin, mais en réalité, ce qu'ils transmettent au travers de ces petites annonces c'est: "Recherche solitude." »

Autrement dit, dans notre culture, certains placent trop d'attentes sur le conjoint. Pour eux, le mariage n'est pas l'union de deux personnes imparfaites qui s'unissent pour créer un lieu de stabilité, d'amour et de consolation, un « havre dans un monde cruel[^43] ». Ils recherchent plutôt quelqu'un qui les acceptera comme ils sont, qui aura des capacités complémentaires aux leurs et qui comblera leurs désirs sexuels et émotionnels. Pour répondre à ces exigences, il faut une femme « romancière/astronaute avec une expérience de top model[^44] », ou l'équivalent chez un homme. Un mariage fondé sur l'accomplissement personnel et non sur l'abnégation exige un conjoint sans besoins ou presque qui, de plus, comblera les vôtres, sans rien demander en retour. Bref, aujourd'hui, on attend beaucoup trop d'un conjoint.

D'autres, au contraire, ont peu d'attentes quant au mariage. Ils en ont même très peur. Tierney pense qu'ils sont plus nombreux dans ce cas, du moins chez ses amis· new-yorkais. Ceux qui rêvent du conjoint parfaitement complémentaire sont moins nombreux que ceux qui n'en veulent pas mais qui ont peut-être du mal à l'admettre. Après tout, notre culture place la liberté, l'autonomie et l'épanouissement en haut de l'échelle des valeurs. Toute personne sensée sait en son for intérieur que n'importe quelle relation d'amour entraîne la perte de ces trois valeurs. Vous pouvez dire: « Je veux quelqu'un qui m'accepte tel que je suis », mais au plus profond de votre cœur vous savez que vous n'êtes pas parfait, qu'il y a des choses à changer en vous, et que quiconque se risquera à vous connaître intimement souhaitera également les changer. Vous savez aussi que l'autre aura des besoins, profonds, et des défauts. Tout ceci semble douloureux, et ça l'est. Vous n'en voulez donc pas. Il est difficile d'admettre publiquement ou à soi-même qu'on ne veut pas se marier. Alors, vous réglez votre Détecteur de défauts au niveau le plus élevé. Ça devrait marcher et tenir le mariage à distance.

Mais l'éviter simplement pour ne pas perdre sa liberté, c'est infliger à son cœur la pire des punitions. C.S. Lewis l'a très bien exprimé:

> Aimez quelque chose, et votre cœur sera certainement déchiré et probablement brisé. Si vous voulez être sûr de le garder intact, abstenez-vous de donner votre cœur, à qui que ce soit, même à un animal. Enveloppez-le soigneusement dans le linceul de vos hobbies et de vos petits luxes; évitez toute forme de relation; enfermez votre cœur à clef dans le cercueil de votre égoïsme. Mais dans ce cercueil - sûr, obscur, immobile, sans air -, il changera. Il ne se brisera pas; il deviendra incassable, impénétrable, indissoluble. L'alternative à la tragédie, ou, du moins, au risque de la tragédie est la damnation[^45].

Pour conclure, dans notre société, nous sommes trop pessimistes quant à la possibilité de la « monogamie » *parce que* nos attentes envers un conjoint sont trop idéalistes. Une mauvaise compréhension des buts du mariage en est la cause.

## On n'épouse jamais la bonne personne

Quelle est la solution? Elle consiste à explorer ce que la Bible dit au sujet du mariage. Nous comprendrons ainsi comment sortir de l'impasse dans laquelle notre culture nous a plongés.

La Bible explique pourquoi la recherche de compatibilité semble à ce point vouée à l'échec. En tant que pasteur, je me suis entretenu avec des milliers de couples. Certains préparaient leur mariage, d'autres travaillaient à son enrichissement et d'autres encore tentaient de le sauver. Je les ai entendu dire à maintes reprises : « Aimer ne *devrait pas* être si difficile; l'amour devrait surgir naturellement ». Ma réponse est toujours plus ou moins la même: « Qu'est-ce qui vous fait croire cela? Est-ce que quelqu'un qui veut devenir footballeur professionnel dirait: "Ça ne devrait pas être si difficile de marquer un but" ? Et quelqu'un qui voudrait écrire le plus grand roman de sa génération: "Ça ne devrait pas être difficile de créer des personnages crédibles et d'inventer une histoire captivante" ? » La réplique classique est la suivante: « Mais il ne s'agit pas de foot ou de littérature. On parle *d'amour*. Il devrait surgir naturellement si deux personnes sont compatibles, si elles sont véritablement des âmes sœurs. »

Du point de vue chrétien, la réponse est qu'*aucun* couple n'est compatible. Selon Stanley Hauerwas, professeur d'éthique de l'Université de Duke:

> La philosophie de la réalisation de soi a un effet destructeur sur le mariage, puisqu'elle part du principe que le mariage et la famille sont avant tout des institutions au service de l'épanouissement personnel, nécessaires pour devenir des êtres humains à part entière et heureux. On part de l'hypothèse que la bonne personne pour nous existe et que si on la cherche bien, on la trouvera. Cette présupposition morale omet un des aspects fondamentaux du mariage. Elle oublie de mentionner qu'on épouse toujours la mauvaise personne.
> On ne connaît jamais l'être qu'on épouse; on croit seulement le connaître. Même si, au départ, c'est la bonne personne, laissez-lui un peu de temps et, à coup sûr, elle changera. Car le mariage, étant [une des plus grandes choses qui soient], implique que nous ne sommes plus le même, une fois que nous nous y sommes engagé. Le problème majeur est [...] d'apprendre à aimer et à prendre soin de l'étranger auquel on s'est uni[^46].

Hauerwas démontre que trouver une âme sœur parfaitement compatible relève de l'impossible. Le mariage crée une proximité entre deux êtres humains, plus intense que toute autre relation. Par conséquent, à l'instant où vous vous mariez, vous et votre époux commencez à changer de façon profonde, sans pouvoir connaître ces changements à l'avance. Vous ne savez donc pas, et vous ne pouvez pas savoir, comment sera votre conjoint tant que vous n'aurez pas atteint ce futur-là.

Cette déclaration a irrité beaucoup de gens. Ce n'est pas surprenant, puisque son intention est de s'inscrire en faux avec l'esprit de notre époque, d'où une généralisation. Il existe évidemment de bonnes raisons de ne pas épouser quelqu'un: une trop grande différence d'âge, ne pas comprendre la langue de l'autre, etc. Le mariage est suffisamment difficile pour ne pas y ajouter des obstacles supplémentaires. La loi de Hauerwas est graduée: Il y a des personnes qu'il ne faut pas épouser, *vraiment* pas. Mais toutes les autres sont de toute façon incompatibles. Tout couple ayant réussi un bon mariage sur le long terme sait de quoi parle Hauerwas. Au fil du temps, on traverse des saisons dans lesquelles on doit apprendre à aimer quelqu'un qui n'a rien à voir avec la personne que l'on a épousée, un inconnu en quelque sorte. L'un et l'autre, vous devrez opérer des changements que vous ne voulez pas faire. Ce voyage peut finalement mener à un mariage solide, tendre et joyeux. Mais ce ne sera pas parce que vous aurez épousé la personne parfaitement compatible. Elle n'existe pas.

La dédicace de ce livre s'adresse à 5 couples d'amis que Kathy et moi connaissons depuis près de 40 ans. Grâce à eux, nous avons pu voir des aspects très intimes d'autres mariages. Une profonde amitié s'est créée entre nous, à l'époque cil' nos études universitaires: les femmes sont devenues de bonnes amies et, de fil en aiguille, leurs maris aussi. Ces relations représentent environ 40 ans de lettres, d'appels téléphoniques, de courriels, de visites, de vacances communes, de peines et de joies partagées. Nous ignorons bien peu de choses sur nos mariages ou nos vies respectives. Une des soirées les plus agréables que nous puissions passer ensemble (à la plage par exemple) consiste à évoquer, avec beaucoup de rires, les débuts de nos relations et les premières années de nos mariages respectifs. Comment avons-nous choisi nos conjoints ? De l'extérieur, cela a dû sembler complètement dingue.

Cindy et Jim: elle était élégante, élevée dans la tradition grecque orthodoxe, discrète, rêveuse et GRECQUE. Jim était plein d'entrain, chamailleur, rigolo et baptiste. Gayle et Gary: en plus de 7 ans d'écart et d'importants différends théologiques, Gary aimait les randonnées de deux semaines avec ses étudiants, alors que pour Gayle, dormir dans un hôtel Holiday Inn s'apparentait déjà à du camping. Louise et David: elle était diplômée en Histoire de l'art et en littérature anglaise, et est restée fidèle à sa confession de protestante réformée. David était pasteur laïc dans une Assemblée de Dieu. Il réveillait tout son dortoir avec des chants de louange. Wayne et Jane: elle le voyait comme de l'or pur, brut, caché sous un physique de joueur de hockey, alors qu'elle reconnaissait être une snob du sud. Doug et Adele: elle était une globe-trotteuse et une missionnaire chevronnée, et lui, un jeune collaborateur au Inter-Varsity Fellowship (N.D.É.: Groupe Biblique Universitaire). Elle venait de vivre une séparation douloureuse avec un homme qui s'appelait aussi Doug. La veille de leur mariage, Adele s'est assise en larmes au pied de leur lit, se demandant si elle faisait le bon choix. Elle témoigne aujourd'hui: « Notre mariage a commencé aux portes du doute et de l'enfer, mais il se trouve aujourd'hui aux portes du paradis. »

Et bien sûr, nous deux. Kathy était presbytérienne, avec des idées très arrêtées, et sûre de vouloir s'impliquer dans un ministère urbain (suite à la lecture du livre *La croix et le poignard* de David Wilkerson). De mon côté, je venais de promettre à l'évêque de ma petite église rurale non presbytérienne que je ne deviendrais *pas* presbytérien, même si j'étudiais dans un séminaire de cette sensibilité.

Dans ces 6 couples, aucun de nous n'offrait la moindre chance à l'autre. Pourtant, nous voilà tous heureux et épanouis ; nous voyons nos enfants se marier et avoir des enfants à leur tour; nous nous aidons mutuellement lors des opérations chirurgicales, des décès de nos parents, et des crises de toutes sortes.

La première raison que donne Hauerwas sur le fait que personne n'est compatible pour le mariage, c'est qu'il nous change profondément. Il existe une autre raison. Chaque conjoint arrive dans le mariage spirituellement brisé par le péché, ce qui signifie, entre autres, qu'il est égocentrique, vivant *incurvatus in se*[^47]. Comme l'a dit l'écrivain Denis de Rougemont: « Pourquoi des personnes névrosées, individualistes, immatures, deviendraient-elles soudainement des anges en tombant amoureuses[^48]? » Il est donc *bien plus* difficile et douloureux de réussir un bon mariage que de réaliser des prouesses athlétiques ou artistiques. Le talent naturel, à l'état brut, ne permet ni de jouer au football comme un professionnel ni de produire de la grande littérature, sans d'abord s'astreindre à une discipline et un travail acharné. À la lumière de ce qui est profondément mauvais dans notre nature humaine, pourquoi serait-il facile de bien vivre dans la tendresse avec un autre être humain? Les stars du sport ou les grands artistes ont souvent lamentablement échoué dans le domaine du mariage. La doctrine biblique du péché explique pourquoi le mariage, plus que toute autre chose bonne et importante de ce monde déchu, est si difficile et douloureux.

## La romance apocalyptique

Le monde moderne exacerbe la difficulté du mariage en l'écrasant sous le poids de ses attentes impossibles et quasi cosmiques. Selon Ernest Becker, auteur et lauréat du prix Pulitzer, la culture moderne a créé le désir de la « romance apocalyptique ». Il fut un temps où nous attendions du mariage et de la famille, amour, soutien et sécurité. Mais pour le sens de la vie, l'espoir en l'avenir, le cadre moral et notre identité, nous nous tournions vers Dieu et l'au-delà. Aujourd'hui, notre culture nous enseigne que personne ne peut être sûr de ces choses, si toutefois elles existent. Donc, soutient Becker, quelque chose doit combler le vide et souvent, ce quelque chose, c'est l'amour romantique. Nous nous attendons à ce que les relations sexuelles et le romantisme nous offrent ce que nous avions l'habitude de recevoir par la foi en Dieu. Il écrit:

> L'amant devient l'idéal divin qui remplit la vie de l'autre. Tous les besoins spirituels et moraux se centrent alors sur un individu. [...] En un mot, l'objet de son amour devient Dieu. [...] L'homme aspire à un « toi » lorsque s'effondre la grande communauté religieuse que Dieu régit. [...][^49] Après tout, que cherchons-nous lorsque nous élevons notre partenaire au rang de dieu? Nous voulons la rédemption - rien de moins[^50].

En tant que pasteur, j'ai entendu des centaines de plaintes faisant état de relations difficiles et d'amours perdus. L'histoire de Jeff et Sue en est un exemple caractéristiques[^51]. Jeff était grand et beau, le genre de compagnon dont Sue avait toujours rêvé. Il était bavard; elle était timide et discrète en public. Elle aimait donc la prestance avec laquelle il dirigeait la conversation lors de réunions. Sue était résolue et orientée vers l'avenir; Jeff avait tendance à « vivre le moment présent ». Avec leurs différences ils semblaient se compléter à la perfection. Intérieurement, Sue était étonnée que quelqu'un d'aussi attirant ait pu tomber amoureux d'elle. Jeff, de son côté, était content d'avoir trouvé une fille en adoration devant lui, alors que tant de femmes lui avaient reproché son manque d'ambition. Après un an de mariage, Sue ne voyait plus dans l'éloquence de Jeff que de l'égocentrisme et une incapacité à écouter. Elle vivait son absence de plan de carrière comme une amère déception. Pour Jeff, la discrétion de Sue s'apparentait à un manque de transparence et derrière sa timidité et sa voix douce, se cachait ce qu'il voyait désormais comme une personnalité autoritaire. Ce mariage s'est vite détérioré pour se terminer par un divorce rapide.

Le désenchantement, la « fin de la lune de miel », est chose courante et l'a été depuis des siècles. C'est normal, voire inéluctable. Toutefois, la profondeur de la désillusion ressentie par les gens à notre époque est un phénomène nouveau, tout comme la vitesse à laquelle les mariages se désagrègent. De nos jours, quelque chose intensifie cette expérience naturelle et la rend toxique. Il s'agit de l'illusion qui nous fait croire que, si nous trouvons notre véritable et unique âme sœur, tout ce qui ne va pas chez nous sera guéri. Cela transforme l'être aimé en Dieu; aucun humain ne peut être à la hauteur d'une telle attente.

Pourquoi alors ne pas se débarrasser du mariage, cet objet culturel d'un autre temps, comme le suggèrent certains? Nos contemporains sont des individus libres et autonomes. Nous avons vu à quel point la famille, les organisations religieuses, les États et nations, toutes les institutions sociales humaines fondamentales, ont été des instruments d'oppression. L'ère du mariage est peut-être terminée. Depuis les années 1970, on a beaucoup prédit l'agonie du mariage en tant qu'institution. Plus récemment, la presse a publié les résultats d'une étude du Pew Research Center, montrant que près de 40 % des Américains pensent que le mariage devient archaïque[^52]. Un des acteurs du film *Monogamy* a dit lors d'une Interview: « Dans ce pays, on a plus ou moins échoué avec le mariage. On cherche trop à protéger cette institution sacrée mais elle est en faillite. Il est temps de créer un nouveau modèle[^53]. »

## Une profonde ambivalence

Le mariage semble être en voie de disparition; c'est en tout cas l'impression générale. Malgré cela, ses détracteurs n'en sont pas tout à fait convaincus et ils se contredisent. Pour citer deux exemples typiques, j'évoque *Contre l'amour: la déroute des sentiments* de Laura Kipnis (éditions la Table Ronde, 2004) et *Mariage, dossier confidentiel: l'ère postromantique des femmes à tout faire, des enfants-rois, des conjoints à la libido en berne et des couples rebelles qui réécrivent les règles*[^54] de Pamela Haag (éditions Harper, 2011). Les deux auteurs passent beaucoup de temps à démontrer que le mariage traditionnel est étouffant et qu'il est pratiquement impossible d'en trouver un qui soit durable et réellement heureux. Pourtant, elles finissent par mentionner, presque à contrecœur, qu'il faut le conserver mais rester très ouvert à des relations sexuelles et à des rencontres en dehors du mariage.

Elissa Strauss, dans sa critique du livre de Mme Haag pour le magazine *Slate*, réplique en disant que l'auteur « ne fournit aucune preuve que ces pionniers des relations non monogames se portent mieux que les autres[^55]. » En effet, les « couples rebelles » décrits par Haag, qui étaient mariés, ont eu des aventures ou ont flirté sur Internet, ont fait le constat d'expériences décevantes, voire nuisibles à leur mariage. « Finalement, conclut Strauss, la loyauté dont Haag fait preuve à l'égard du mariage est d'autant plus surprenante [...] qu'elle ne fait que le démolir[^56]. » Cela résume joliment l'ambiguïté actuelle des détracteurs du mariage face à cette institution.

Il existe peu d'arguments sérieux, voire aucun, pour conforter la thèse actuelle d'une société viable sans mariage. Même les critiques de la monogamie doivent admettre, au moins de façon pragmatique, qu'on ne peut vivre sans[^57]. Cela provient en partie de la recherche expérimentale en pleine croissance, à laquelle nous nous référons dans ce chapitre[^58]. Les preuves s'accumulent concernant le mariage traditionnel et exclusivement monogame: il est source d'énormes avantages, de toutes sortes, pour les adultes et plus encore pour les enfants et la société dans son ensemble.

Il est cependant inutile de s'appuyer sur la recherche scientifique pour découvrir que le mariage s'inscrit dans la durée. Son omniprésence parle d'elle-même. Dans l'état actuel de nos connaissances, le mariage a toujours été au cœur de la vie humaine, quelle que soit la culture ou l'époque[^59]. Bien que le nombre de personnes mariées ait chuté dans notre culture occidentale, le pourcentage de ceux qui espèrent se marier n'a en rien diminué. Nous aspirons profondément au mariage. Le cri d'Adam à la vue d'Ève: « Enfin! », est révélateur. Nous avons la profonde conviction qu'il recèle un trésor indescriptible. Et c'est vrai. Le problème ne vient pas du mariage lui-même. Selon la Genèse, chapitres 1 et 2, nous avons été faits pour le mariage et il a été fait pour nous. Genèse 3 nous explique que le mariage, comme tous les autres aspects de la vie humaine, a été brisé par le péché.

Si nous avons une vision trop romantique et idéaliste du mariage, nous sous-estimons l'influence du péché sur les êtres humains. Si notre idée du mariage est trop pessimiste et cynique, nous comprenons mal son origine divine. Et pour peu que nous voulions concilier les deux, comme le fait notre culture moderne, nous sommes doublement accablés par une conception déformée. Le problème ne vient pas de l'institution du mariage, mais de nous-mêmes.

## Le grand secret

Le mariage est un « grand mystère»: c'est la déclaration de Paul mentionnée en début de chapitre que nous avons rappelée de bien des manières. Nous ne pouvons le mettre au rebut, car il est trop important, mais en même temps le mariage nous dépasse. Toutefois, le mot grec employé par Paul, *mysterion*, couvre un champ lexical qui inclut également l'idée de « secret ». La Bible ne l'utilise pas pour désigner une sorte de connaissance ésotérique réservée aux seuls initiés, mais plutôt pour exprimer une vérité insoupçonnée et merveilleuse que Dieu nous révèle par son Esprit[^60]. Paul utilise ce mot ailleurs, en se référant à d'autres révélations du plan de salut de Dieu dans l'Évangile. Mais dans Éphésiens 5, de façon surprenante, il applique au mariage ce terme riche de sens. Au verset 31, il cite la fin du récit du premier mariage, dans la Genèse: « L'homme quittera son père et sa mère pour s'attacher à sa femme et les deux ne seront plus qu'une seule chair ». Puis il ajoute, littéralement que ceci est un *mega-mysterion* (verset 32): une vérité immense, merveilleuse et profonde, qu'on ne peut comprendre qu'avec l'aide de l'Esprit de Dieu.

Mais *quel est donc* le secret du mariage? Paul poursuit: « je dis cela par rapport à Christ et à l'Église » (Colombe). Il fait écho à sa déclaration au verset 25: « Vous, maris, aimez vos femmes comme le Christ a aimé l'Église: il a donné sa vie pour elle ». Bref, le « secret » n'est pas dans le mariage en lui-même, mais constitue le message appelant les maris à faire pour leurs femmes ce que Jésus a fait pour nous amener à l'union avec lui. Et qu'a-t-il fait?

Jésus a *donné sa vie* pour nous. Jésus le Fils, bien qu'il soit égal au Père, a renoncé à sa gloire et a revêtu notre nature humaine (Philippiens 2.5 ss). Bien plus, il est allé volontairement à la croix pour payer le salaire de nos péchés. Il a ôté notre culpabilité et notre condamnation afin que nous puissions être unis à lui (Romains 6.5) et revêtir ainsi sa nature (2 Pierre 1.4). Il a renoncé à sa gloire et à son pouvoir pour devenir un serviteur. Il est mort à ses propres intérêts pour se préoccuper à la place de nos intérêts et besoins (Romains 15.1-3). Jésus nous a servis jusqu'à se sacrifier, ce qui nous a amenés à vivre une profonde union: nous en lui et lui en nous. Et *ça*, dit Paul, c'est la clé pour comprendre le mariage, mais surtout pour le vivre. Paul peut ainsi rattacher la déclaration originale de Genèse 2 sur le mariage à Jésus et à l'Église. Comme l'a dit un commentateur: « Paul a vu qu'au moment où Dieu concevait le mariage originel, il songeait déjà à Christ et à l'Église. Voici l'un des grands desseins que Dieu a prévu pour le mariage: représenter la relation entre Christ et son peuple racheté à jamais[^61] ! »

Nous pouvons donc répondre de façon ferme à ceux qui affirment que le mariage est par nature asservissant et donc obsolète. Dans Philippiens 2, Paul nous dit que le Fils de Dieu n'a pas profité de son égalité avec le Père, mais qu'il a révélé sa grandeur par sa volonté de devenir le serviteur du Père. Il est allé à la croix, mais le Père l'a ressuscité d'entre les morts.

> Ceci nous montre le caractère de Dieu. [...] Le Père, le Fils et le Saint-Esprit ne se manipulent pas les uns les autres pour arriver à leurs fins. [...] Il n'y a ni conquête de l'unité par la diversité, ni diversité par l'unité. Les trois ne font qu'un, et le un est trois[^62].

Mais nous ne pouvons en rester là. Dans Éphésiens 5, Paul nous montre que même sur Terre, Jésus n'a pas utilisé son pouvoir pour nous opprimer, mais a tout sacrifié pour nous amener à l'union avec lui. Cela nous fait passer de la philosophie à une application personnelle et concrète. Si Dieu pensait à l'Évangile du salut par Jésus en fondant le mariage, cela signifie que le mariage ne « fonctionne » que lorsqu'il est au plus près du modèle de l'amour sacrificiel de Dieu incarné par le Christ. Paul répond ainsi à l'objection du caractère oppressif et contraignant du mariage. Il combat également l'idée que les exigences du mariage sont trop élevées. Il y a tant à faire, nous ne savons par où commencer. Commence par ceci, dit Paul: Fais pour ton conjoint ce que Dieu a fait pour toi par Jésus. Le reste suivra.

Voilà le secret: l'Évangile de Jésus et le mariage s'expliquent l'un par l'autre. Quand Dieu a inventé le mariage, il pensait déjà à l'œuvre salvatrice de Jésus.

## Rejetons les fausses options

Nous devrions rejeter l'alternative que nous proposent les deux modèles du mariage, traditionnel et contemporain. Le but du mariage est-il de renoncer à ses propres intérêts pour le bien de la famille, ou au contraire de les défendre pour son épanouissement personnel? La conception chrétienne ne propose pas de choix entre les deux; elle offre plutôt l'épanouissement mutuel grâce au sacrifice mutuel. Jésus a donné sa vie, il est mort à lui-même pour nous sauver et nous faire siens. Nous nous donnons à notre tour, nous mourons à nous-mêmes, tout d'abord lorsque nous nous repentons et croyons à l'Évangile, puis, lorsque nous nous soumettons à sa volonté, jour après jour. Nous placer sous son autorité nous apporte une sécurité totale, puisqu'il a déjà montré qu'il était prêt à aller en enfer et à en revenir pour nous. Le savoir chasse nos craintes de perdre notre identité en nous soumettant par amour.

Que faut-il pour faire « fonctionner » un mariage? Il faut en connaître le secret, l'Évangile, et comprendre qu'il vous donne la puissance et le modèle à suivre pour votre mariage. D'une part, l'expérience du mariage vous dévoilera la beauté et les profondeurs de l'Évangile et vous conduira à davantage dépendre de lui. D'autre part, une meilleure compréhension de l'Évangile vous aidera à vivre une union toujours plus profonde l'un avec l'autre au fil du temps.

Voici donc le message de ce livre: « le mystère de l'Évangile est dévoilé[^63] » à travers le mariage. L'Évangile vous aide à comprendre le mariage et le mariage vous aide à comprendre l'Évangile. Le mariage est le principal vecteur de l'Évangile pour renouveler en profondeur votre cœur et transformer votre vie.

La raison pour laquelle le mariage est si douloureux et pourtant merveilleux, c'est parce qu'il reflète l'Évangile qui est en même temps douloureux et merveilleux. Voici le message de l'Évangile: nous sommes bien plus pécheurs et imparfaits que nous n'oserions l'avouer, mais nous sommes également bien plus aimés et acceptés en Jésus-Christ que nous n'oserions l'espérer. Il s'agit du seul genre de relation susceptible de nous transformer réellement. L'amour sans la vérité est sentimental. Il nous soutient et nous réconforte, mais nous maintient dans le déni quant à nos défauts. La vérité dépourvue d'amour n'est que dureté dans la mesure où on ne peut vraiment entendre l'information donnée. Mais l'amour salvateur de Dieu en Christ se démarque à la fois par une honnêteté absolue concernant notre identité et par un engagement tout aussi absolu et inconditionnel envers nous. Ce dernier est miséricordieux et nous permet d'affronter la vérité nous concernant, et de nous repentir. La conviction de péché et la repentance nous poussent à nous accrocher à la grâce et à la miséricorde de Dieu, sources de repos.

Les périodes difficiles de la vie conjugale nous poussent à connaître davantage cet amour transformateur de Dieu. Dans le cadre d'un bon mariage, nous en ferons en plus l'expérience sur le plan humain. L'Évangile peut remplir nos cœurs de l'amour de Dieu et nous aider à gérer les manques d'amour de notre conjoint. Cela nous libère du regard critique concernant ses péchés et ses défauts, ainsi que du besoin d'en parler. Cela nous permet de continuer de l'aimer et de l'accepter tel qu'il est. Et quand, par la puissance de l'Évangile, notre conjoint ressent cet amour honnête et attentif, il peut, le moment venu, amorcer le changement dans sa propre vie.

Voilà le grand secret! Avec l'Évangile, nous recevons à la fois la puissance et le modèle nécessaires à l'aventure du mariage. Mais il reste bien des choses à dire sur leur fonctionnement. Revenons donc à Éphésiens 5, afin d'approfondir notre compréhension de ce grand secret.


"""
