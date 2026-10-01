// Some definitions presupposed by pandoc's typst output.
#let blockquote(body) = [
  #set text( size: 0.92em )
  #block(inset: (left: 1.5em, top: 0.2em, bottom: 0.2em))[#body]
]

#let horizontalrule = line(start: (25%,0%), end: (75%,0%))

#let endnote(num, contents) = [
  #stack(dir: ltr, spacing: 3pt, super[#num], contents)
]

#show terms: it => {
  it.children
    .map(child => [
      #strong[#child.term]
      #block(inset: (left: 1.5em, top: -0.4em))[#child.description]
      ])
    .join()
}

// Some quarto-specific definitions.

#show raw.where(block: true): set block(
    fill: luma(230),
    width: 100%,
    inset: 8pt,
    radius: 2pt
  )

#let block_with_new_content(old_block, new_content) = {
  let d = (:)
  let fields = old_block.fields()
  fields.remove("body")
  if fields.at("below", default: none) != none {
    // TODO: this is a hack because below is a "synthesized element"
    // according to the experts in the typst discord...
    fields.below = fields.below.abs
  }
  return block.with(..fields)(new_content)
}

#let empty(v) = {
  if type(v) == str {
    // two dollar signs here because we're technically inside
    // a Pandoc template :grimace:
    v.matches(regex("^\\s*$")).at(0, default: none) != none
  } else if type(v) == content {
    if v.at("text", default: none) != none {
      return empty(v.text)
    }
    for child in v.at("children", default: ()) {
      if not empty(child) {
        return false
      }
    }
    return true
  }

}

// Subfloats
// This is a technique that we adapted from https://github.com/tingerrr/subpar/
#let quartosubfloatcounter = counter("quartosubfloatcounter")

#let quarto_super(
  kind: str,
  caption: none,
  label: none,
  supplement: str,
  position: none,
  subrefnumbering: "1a",
  subcapnumbering: "(a)",
  body,
) = {
  context {
    let figcounter = counter(figure.where(kind: kind))
    let n-super = figcounter.get().first() + 1
    set figure.caption(position: position)
    [#figure(
      kind: kind,
      supplement: supplement,
      caption: caption,
      {
        show figure.where(kind: kind): set figure(numbering: _ => numbering(subrefnumbering, n-super, quartosubfloatcounter.get().first() + 1))
        show figure.where(kind: kind): set figure.caption(position: position)

        show figure: it => {
          let num = numbering(subcapnumbering, n-super, quartosubfloatcounter.get().first() + 1)
          show figure.caption: it => {
            num.slice(2) // I don't understand why the numbering contains output that it really shouldn't, but this fixes it shrug?
            [ ]
            it.body
          }

          quartosubfloatcounter.step()
          it
          counter(figure.where(kind: it.kind)).update(n => n - 1)
        }

        quartosubfloatcounter.update(0)
        body
      }
    )#label]
  }
}

// callout rendering
// this is a figure show rule because callouts are crossreferenceable
#show figure: it => {
  if type(it.kind) != str {
    return it
  }
  let kind_match = it.kind.matches(regex("^quarto-callout-(.*)")).at(0, default: none)
  if kind_match == none {
    return it
  }
  let kind = kind_match.captures.at(0, default: "other")
  kind = upper(kind.first()) + kind.slice(1)
  // now we pull apart the callout and reassemble it with the crossref name and counter

  // when we cleanup pandoc's emitted code to avoid spaces this will have to change
  let old_callout = it.body.children.at(1).body.children.at(1)
  let old_title_block = old_callout.body.children.at(0)
  let old_title = old_title_block.body.body.children.at(2)

  // TODO use custom separator if available
  let new_title = if empty(old_title) {
    [#kind #it.counter.display()]
  } else {
    [#kind #it.counter.display(): #old_title]
  }

  let new_title_block = block_with_new_content(
    old_title_block, 
    block_with_new_content(
      old_title_block.body, 
      old_title_block.body.body.children.at(0) +
      old_title_block.body.body.children.at(1) +
      new_title))

  block_with_new_content(old_callout,
    block(below: 0pt, new_title_block) +
    old_callout.body.children.at(1))
}

// 2023-10-09: #fa-icon("fa-info") is not working, so we'll eval "#fa-info()" instead
#let callout(body: [], title: "Callout", background_color: rgb("#dddddd"), icon: none, icon_color: black, body_background_color: white) = {
  block(
    breakable: false, 
    fill: background_color, 
    stroke: (paint: icon_color, thickness: 0.5pt, cap: "round"), 
    width: 100%, 
    radius: 2pt,
    block(
      inset: 1pt,
      width: 100%, 
      below: 0pt, 
      block(
        fill: background_color, 
        width: 100%, 
        inset: 8pt)[#text(icon_color, weight: 900)[#icon] #title]) +
      if(body != []){
        block(
          inset: 1pt, 
          width: 100%, 
          block(fill: body_background_color, width: 100%, inset: 8pt, body))
      }
    )
}



#let article(
  title: none,
  subtitle: none,
  authors: none,
  date: none,
  abstract: none,
  abstract-title: none,
  cols: 1,
  margin: (x: 1.25in, y: 1.25in),
  paper: "us-letter",
  lang: "en",
  region: "US",
  font: "libertinus serif",
  fontsize: 11pt,
  title-size: 1.5em,
  subtitle-size: 1.25em,
  heading-family: "libertinus serif",
  heading-weight: "bold",
  heading-style: "normal",
  heading-color: black,
  heading-line-height: 0.65em,
  sectionnumbering: none,
  pagenumbering: "1",
  toc: false,
  toc_title: none,
  toc_depth: none,
  toc_indent: 1.5em,
  doc,
) = {
  set page(
    paper: paper,
    margin: margin,
    numbering: pagenumbering,
  )
  set par(justify: true)
  set text(lang: lang,
           region: region,
           font: font,
           size: fontsize)
  set heading(numbering: sectionnumbering)
  if title != none {
    align(center)[#block(inset: 2em)[
      #set par(leading: heading-line-height)
      #if (heading-family != none or heading-weight != "bold" or heading-style != "normal"
           or heading-color != black or heading-decoration == "underline"
           or heading-background-color != none) {
        set text(font: heading-family, weight: heading-weight, style: heading-style, fill: heading-color)
        text(size: title-size)[#title]
        if subtitle != none {
          parbreak()
          text(size: subtitle-size)[#subtitle]
        }
      } else {
        text(weight: "bold", size: title-size)[#title]
        if subtitle != none {
          parbreak()
          text(weight: "bold", size: subtitle-size)[#subtitle]
        }
      }
    ]]
  }

  if authors != none {
    let count = authors.len()
    let ncols = calc.min(count, 3)
    grid(
      columns: (1fr,) * ncols,
      row-gutter: 1.5em,
      ..authors.map(author =>
          align(center)[
            #author.name \
            #author.affiliation \
            #author.email
          ]
      )
    )
  }

  if date != none {
    align(center)[#block(inset: 1em)[
      #date
    ]]
  }

  if abstract != none {
    block(inset: 2em)[
    #text(weight: "semibold")[#abstract-title] #h(1em) #abstract
    ]
  }

  if toc {
    let title = if toc_title == none {
      auto
    } else {
      toc_title
    }
    block(above: 0em, below: 2em)[
    #outline(
      title: toc_title,
      depth: toc_depth,
      indent: toc_indent
    );
    ]
  }

  if cols == 1 {
    doc
  } else {
    columns(cols, doc)
  }
}

#set table(
  inset: 6pt,
  stroke: none
)

#show: doc => article(
  pagenumbering: "1",
  toc_title: [Table of contents],
  toc_depth: 3,
  cols: 1,
  doc,
)

= Theoretical and empirical background
<theoretical-and-empirical-background>
== The socialization of meritocracy at school
<the-socialization-of-meritocracy-at-school>
Meritocracy refers to a distributive system in which individual merit, typically defined as effort and personal ability, is treated as the primary criterion for allocating resources and rewards, rather than social origins or inherited privilege @young_rise_1958@bell_equality_1972. While #cite(<young_rise_1958>, form: "prose") coined the term as a dystopian critique, it has since been re-appropriated as a positive ideal of fairness in liberal and market-oriented societies @mijs_paradox_2021@vandewerfhorst_meritocracy_2024. From a sociological standpoint, meritocracy operates both as a cognitive judgment about how inequality works and as a moral lens through which people evaluate whether unequal outcomes are deserved @castillo_meritocracia_2019@heuer_legitimizing_2020. Precisely because it frames outcomes as earned, meritocratic ideals have been largely criticized as they could lead to reinforce inequality: "winners" are encouraged to see their position as deserved, while "losers" are pushed toward self-blame rather than structural critique @garcia-sierra_dark_2023@sandel_tyranny_2020. In this line, research in adult populations has documented systematic associations between meritocratic beliefs and the justification of social inequalities @mijs_paradox_2019@castillo_perceptions_2025@tejero-peregrina_perceived_2025@liu_does_2025. Despite the centrality of meritocratic beliefs for the understanding of the social order, the question of how and when they develop, and whether such beliefs are possible to identify in early socialization stages has received far less attention.

Schools are key institutions when it comes to the socialization of meritocracy. On the one hand, they advance meritocratic ideals through narratives of effort, achievement, self-improvement, and education as a legitimate route to rewards and social mobility @batruch_belief_2022@chauvin_school_2026@darnon_where_2018@wiederkehr_belief_2015. On the other hand, they embed these ideals in routine practices of grading, tracking, selection, awards, and credentialing, which operate as institutional markers of worth that make merit visible and consequential in ways that are immediate and socially meaningful @resh_sense_2014@traini_stratification_2022. This dual character, at once cultural promotion and institutional enactment, is what makes schools a particularly important arena for understanding how meritocratic beliefs are formed and contested.

Along meritocratic socialization, schools are settings where privilege-based advantages, including family resources, cultural capital, peer environments, and unequal school quality, are also visible and consequential @batruch_belief_2022@goudeau_hidden_2017a. This duality can give rise to ambivalent or internally differentiated orientations. Students may endorse meritocracy as a normative ideal while simultaneously recognizing structural constraints, reflecting what the literature describes as dual consciousness @tang_meritocratic_2025@liu_does_2025. This coexistence is not fixed but can shift with educational experience: #cite(<liu_does_2025>, form: "prose") argue that education can operate as either legitimation or enlightenment depending on the broader inequality context, and longitudinal evidence suggests that recognition of structural advantage tends to grow as students progress through schooling @tang_meritocratic_2025. More broadly, meritocratic framing encourages students across social backgrounds to interpret success and failure in individual terms, reinforcing personal responsibility while limiting attention to structural constraints @darnon_where_2018. As #cite(<lampert_meritocratic_2013>, form: "prose") argues, meritocratic schooling can reward a minority while leading many others to internalize failure as a personal deficiency, and even students from disadvantaged backgrounds often adopt meritocratic narratives, suggesting that prolonged exposure to school-based evaluation is a powerful mechanism shaping beliefs about justice and inequality @henry_development_2006.

The dynamics of meritocratic socialization are not static. Developmental research suggests that by around age 10, children already articulate judgments about social differences, typically emphasizing individual, merit-based explanations while only beginning to incorporate opportunity-based accounts @sigelman_age_2013a@imhoff_nociones_2025. As students move through the school system, however, cumulative exposure to grading, tracking, peer comparison, and visible socioeconomic gaps can make the limits of effort as a sufficient explanation for success increasingly apparent. #cite(<tang_meritocratic_2025>, form: "prose") show that among Chinese college students, those from lower socioeconomic backgrounds initially report lower recognition of privilege-based advantages than their higher-SES peers, but this gap narrows progressively across college years as both groups converge toward stronger acknowledgment of structural advantage. This trajectory suggests something important: it is not simply that older students believe less in meritocracy, but that sustained exposure to the educational system makes structural advantage harder to ignore, generating what has been described as a "reality shock" in which meritocratic discourse increasingly clashes with experienced inequality @allen_top_2016. Adolescence is therefore a particularly consequential period for studying these processes, precisely when abstract reasoning about justice and fairness becomes more sophisticated and school-based sorting mechanisms grow more salient @henry_development_2006@resh_sense_2014.

The consequences of these beliefs extend well beyond individual attitudes. Students who perceive schools as highly meritocratic, particularly believing that effort and talent are fairly rewarded, are more likely to justify unequal access to social services, perceive class inequalities as less unjust, express weaker support for redistribution, and endorse stronger system-justifying beliefs @castillo_socialization_2024@batruch_belief_2022@wiederkehr_belief_2015. The effects are not confined to students: among teachers, belief in school meritocracy predicts preference for competitive over cooperative classroom practices, while among parents it predicts lower demand for policies aimed at reducing socioeconomic inequalities in achievement @darnon_competitive_2023. These patterns suggest that meritocratic beliefs are not merely attitudinal outcomes of schooling but active inputs into the educational environment itself, shaping classroom dynamics, institutional decisions, and the broader normative climate in which students develop.

== Conceptualizing and measuring meritocratic and privilege beliefs
<conceptualizing-and-measuring-meritocratic-and-privilege-beliefs>
The research agenda on meritocratic beliefs has faced challenges in conceptualizing and measuring meritocracy. Some studies have relied on narrow operationalizations that equate meritocracy with the social valuation of effort @mijs_paradox_2021@wiederkehr_belief_2015, often reflecting simplified readings of Young's #cite(<young_rise_1958>, form: "year") formulation (Merit = Effort + Intelligence). Other approaches treat meritocratic beliefs as interchangeable with broader constructs such as system justification @day_movin_2017@jost_decade_2004a, beliefs about social mobility @deng_its_2025@mccoy_priming_2007, or generalized support for equal opportunity @batruch_belief_2022@darnon_where_2018. As a result, attempts to measure meritocracy with items battery frequently collapse indicators that tap different dimensions, making it difficult to assess what meritocracy actually means in practice.

A related, more specific limitation to study meritocracy concerns its links with privilege-based factors such as family wealth, social connections, and other structural advantages. As #cite(<castillo_multidimensional_2023>, form: "prose") notes, much survey research relies on item batteries that ask respondents to rate the importance of effort, talent, family background, networks, or luck for "getting ahead", and then reduces these into a single score. This becomes especially problematic when researchers construct continuum measures in composite indicators, treating meritocratic and privilege-based explanations as opposite ends of the same scale @reynolds_perceptions_2014a. Such operationalizations embed a strong zero-sum assumption, namely that endorsing merit necessarily entails rejecting structural or relational advantages, thereby ruling out, by design, the empirical possibility that both types of beliefs coexist. Recent debates, including the exchange between #cite(<mijs_visualizing_2026>, form: "prose") and #cite(<wiesner_effort_2026>, form: "prose");, underscore this point by arguing for measures that capture meritocratic beliefs alongside privilege-based ones, rather than as a residual of the relative importance assigned to structural factors. Complementary empirical work has raised similar concerns: #cite(<liu_does_2025>, form: "prose") shows that meritocratic endorsement may remain stable even as recognition of structural constraints increases; #cite(<tang_meritocratic_2025>, form: "prose") motivates dual consciousness but notes that operationalizations forcing relative trade-offs risk obscuring it; #cite(<kwon_multidimensionality_2024>, form: "prose") identify three attitudinal clusters in Europe, instrumental, idealized merit, and ambivalent, showing that support for merit can coexist with recognition of social connections in distinct configurations; and #cite(<zhu_meritocratic_2025>, form: "prose") argues directly that meritocratic and structural explanations are not zero-sum but often accumulate rather than replace one another. Unidimensional indices, and especially subtraction scores, can therefore manufacture artificial neutral positions and blur substantively distinct belief profiles that carry different educational consequences.

In response to these limitations for the comprehensive measurement of merit and privilege beliefs, #cite(<castillo_multidimensional_2023>, form: "prose") proposed a multidimensional framework that decomposes meritocratic beliefs along two analytically independent axes: preferences versus perceptions, and meritocratic versus privilege-based (originally called "non-meritocratic") allocation principles. Preferences capture normative ideals about how rewards should be distributed (e.g., whether effort and ability ought to determine life chances); perceptions capture descriptive evaluations of how society actually works (e.g., whether observed inequalities reflect merit-based processes) @janmaat_subjective_2013. Meritocratic elements (effort, talent) and privilege-based elements (family wealth, networks) are treated as distinct components rather than as opposite poles of a single continuum. This yields four non-redundant constructs, allowing belief combinations that older measures tend to collapse, such as endorsing meritocracy as an ideal while simultaneously recognizing the weight of family background and connections. The proposal is deliberately minimalist, making it well-suited for large-scale surveys with limited questionnaire space, though this parsimony entails a trade-off: each factor is measured with only two items, which can constrain reliability and content coverage.

Evidence from adult samples using confirmatory factor analysis supports a four-factor structure (perceptions and preferences of both meritocracy and privilege @castillo_multidimensional_2023. Besides, it shows systematic associations among dimensions: stronger perceived privilege tends to co-occur with stronger meritocratic preferences @castillo_multidimensional_2023, a pattern that aligns with Zhu's #cite(<zhu_meritocratic_2025>, form: "year") notion of dual consciousness, here understood as the simultaneous endorsement of meritocracy as a normative ideal alongside descriptive recognition that rewards are shaped by structural advantage. Regarding school-based research, these measurement challenges have received limited attention. #cite(<chauvin_school_2026>, form: "prose") propose a multidimensional measure of school meritocracy that distinguishes between equity and ability dimensions, but their scale applies only to teachers and remains restricted to perceived meritocracy. #cite(<castillo_socialization_2024>, form: "prose") apply the four-factor framework to a sample of Chilean 10th graders and report that students differentiate perceptions from preferences and meritocratic from privilege-based principles; however, that study relies on item-level descriptives and bivariate associations rather than a formal factorial measurement model. Beyond this, to our knowledge no study has examined whether these dimensions are stable across age cohorts or over time, which limits understanding of how meritocratic beliefs develop through schooling and whether observed differences across stages reflect substantive change or measurement non-equivalence.

== This study
<this-study>
Building on the previous evidence, we state the following research hypotheses:

#strong[$H_1$ (Dimensional differentiation)];: Students' meritocratic and privilege beliefs can be represented and measured by four distinct factors: perceived meritocracy, perceived privilege, meritocratic preferences, and privilege acceptance.

#strong[$H_2$ (Beliefs compatibility)];: merit and privilege beliefs are not mutually exclusive, but rather compatible orientations that can coexist within the same individual.

#strong[$H_3$ (Equivalence)];: The four-factor model is equivalent across the two waves of the panel survey and across two age cohorts (6th and 9th grade in the first wave).

This second hypothesis is particularly important because it challenges the assumption that meritocratic and privilege-based beliefs are zero-sum, and it motivates the use of a multidimensional measurement model that allows for the coexistence of these orientations. Nevertheless, it opens the question of how these beliefs are organized within people's minds, and whether they can be grouped into distinct profiles. Individuals do not always adhere to a single ideal, but can apply mutiple beliefs simultaneously. In this regard, while we expect that meritocratic and privilege-based beliefs are compatible, it is possible that some students endorse both types of beliefs strongly, while others endorse one type more than the other. This raises the possibility of identifying different groups of students based in their belief profiles, which could shed new light on how beliefs about merit and privilege are formed among the student population. Thus, as a additional analysis, we will test whether there are different profiles of beliefs regarding meritocracy and privilege using Latent Class Analysis, a method that allows us to identify and group individuals with similar beliefs based on their response patterns.

 
  
#set bibliography(style: "../input/bib/apa6.csl") 


#bibliography("../input/bib/merit-factorial.bib")

