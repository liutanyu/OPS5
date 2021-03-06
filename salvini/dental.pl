/*  DENTAL.PL  */
/*  Shelved on the 3rd of October 1988  */


/**************************************************************************/
/************************ RULE_BASE: dental *******************************/
/**************************************************************************

    This program may be used and amended for non-profit making
    purposes on condition that this header be left intact and in situ.

    Copyright: Steven Salvini & Denis Kinane 1986

**************************************************************************/
 
/**************************************************************************
 
    ruleX:
    Classical 'if-then' rules using Object-Attribute-Value triples
 
**************************************************************************/

rule1:
    if
            pain/duration/constant
        and
            pain/localisation/accurate
        and
            pain/'initiated by'/'gentle touch'
        and
            pain/character/'sudden onset'
        and
            pain/character/'intense & throbbing'
        and
            gingivae/swelling/present
        and
            gingivae/erythema/present
        and
            miscellaneous/'local lymphadenopathy'/present
        and
            pain/'initiated by'/'clenching, biting or chewing'
        and
            tooth/mobility/present
    then
        diagnosis/group/abscess
    with
        certainty(0.9).

rule2:
    if
            false pain/'initiated by'/cold
        and
            pain/'initiated by'/heat
        and
            false pain/'initiated by'/sweet
        and
            diagnosis/group/abscess
        and
            tooth/'recent fillings'/present
        and
            tooth/'TTP'/positive
        and
            tooth/'pulp test'/positive
    then
        patient/diagnosis/'Acute Periapical Abscess'
    with
        certainty(0.9).

rule3:
    if
            diagnosis/group/abscess
        and
            tooth/'TTP'/positive
    then
        patient/diagnosis/'Acute Periapical Abscess'
    with
        certainty(0.6).

rule4:
    if
            diagnosis/group/abscess
        and
            false pain/'initiated by'/cold
        and
            pain/'initiated by'/heat
        and
            false pain/'initiated by'/sweet
        and
            tooth/'TTP'/positive
        and
            tooth/'pulp test'/positive
        and
            swelling/site/'over root apex'
        or
            pain/character/'intense & throbbing'
        or
            pain/character/'severe & stabbing'
        or
            tooth/'carious cavity'/present
        or
            miscellaneous/'previous history'/pulpitis
    then
        patient/diagnosis/'Acute Periapical Abscess'
    with
        certainty(0.8).

rule5:
    if
            diagnosis/group/abscess
        and
            pocketing/depth/'>5mm'
        and
            miscellaneous/sinus/present
        and
            pain/character/dull
        or
            pain/character/'intense & throbbing'
    then
        patient/diagnosis/'Acute Lateral Abscess'
    with
        certainty(0.6).


rule6:
    if
            diagnosis/group/abscess
        and
            pocketing/depth/'>5mm'
        and
            miscellaneous/sinus/present
        and
            pain/character/'sudden onset'
        or
            tooth/'pulp test'/positive
    then
        patient/diagnosis/'Acute Lateral Abscess'
    with
        certainty(0.9).

rule7:
    if
            pain/'initiated by'/cold
        and
            pain/'initiated by'/heat
        and
            pain/'initiated by'/sweet
        and
            pain/localisation/poor
        and
            pain/'initiated by'/'clenching, biting or chewing'
        and
            tooth/'carious cavity'/present
        and
            miscellaneous/'local lymphadenopathy'/present
        and
            gingivae/swelling/present
        and
            tooth/'TTP'/negative
        and
            tooth/'pulp test'/positive
    then
        diagnosis/group/pulpitis
    with
        certainty(0.9).

rule8:
    if
            tooth/'recent fillings'/present
    then
        diagnosis/group/pulpitis
    with
        certainty(0.3).

rule9:
    if
            tooth/'recent fillings'/present
        and
            tooth/'polished high-spot on filling'/present
    then
        diagnosis/group/pulpitis
    with
        certainty(0.4).

rule10:
    if
            diagnosis/group/pulpitis
        and
            pain/duration/constant
        or
            pain/character/'intense & throbbing'
        or
            pain/character/'severe & stabbing'
        or
            false pain/'relieved by'/analgesics
        or
            pain/'relieved by'/'application of cold'
    then
        patient/diagnosis/'Acute Pulpitis'
    with
        certainty(0.8).

rule11:
    if
            diagnosis/group/pulpitis
        and
            pain/character/'spontaneous onset'
        and
            pain/duration/intermittent
        or
            pain/character/dull
    then
        patient/diagnosis/'Chronic Pulpitis'
    with
        certainty(0.7).

rule12:
    if
            pain/duration/intermittent
        and
            pain/localisation/accurate
        and
            pain/character/dull
        and
            pain/'initiated by'/'clenching, biting or chewing'
        and
            gingivae/swelling/present
        and
            gingivae/erythema/present
        and
            tooth/'pulp test'/positive
        and
            miscellaneous/trismus/present
    then
        patient/diagnosis/'Pericoronitis'
    with
        certainty(0.8).

rule13:
    if
            gingivae/swelling/present
        and
            gingivae/erythema/present
        and
            false pain/site/gingivae
        and
            gingivae/bleeding/present
    then
        patient/diagnosis/'Chronic Marginal Gingivitis'
    with
        certainty(0.4).

rule14:
    if
            gingivae/swelling/present
        and
            gingivae/erythema/present
        and
            false pain/site/gingivae
        and
            gingivae/bleeding/present
        and
            pocketing/depth/'<3mm'
    then
        patient/diagnosis/'Chronic Marginal Gingivitis'
    with
        certainty(0.7).

rule15:
    if
            gingivae/swelling/present
        and
            gingivae/erythema/present
        and
            false pain/site/gingivae
        and
            gingivae/bleeding/present
        and
            pocketing/depth/'<3mm'
        or
            tooth/'gingival recession'/present
        or
            miscellaneous/'bad taste'/present
        or
            miscellaneous/halitosis/present
    then
        patient/diagnosis/'Chronic Marginal Gingivitis'
    with
        certainty(0.9).

rule16:
    if
            pocketing/depth/'>3mm & <5mm'
        and
            tooth/'TTP'/negative
        and
            false pain/site/tooth
        and
            false pain/site/gingivae
        and
            'x-ray findings'/'alveolar bone loss'/present
    then
        patient/diagnosis/'Chronic Periodontitis Simplex'
    with
        certainty(0.7).

rule17:
    if
            pocketing/depth/'>3mm & <5mm'
        and
            tooth/'TTP'/positive
        and
            'x-ray findings'/'alveolar bone loss'/present
        and
            tooth/'gingival recession'/present
        and
            pain/site/tooth
        or
            pain/site/gingivae
    then
        patient/diagnosis/'Chronic Periodontitis Simplex'
    with
        certainty(0.7).

rule18:
    if
            pocketing/depth/'>3mm & <5mm'
        and
            tooth/'TTP'/negative
        and
            'x-ray findings'/'alveolar bone loss'/present
        and
            false pain/site/tooth
        and
            false pain/site/gingivae
        and
            tooth/mobility/present
        or
            tooth/migration/present
        or
            gingivae/bleeding/present
        or
            gingivae/swelling/present
        or
            gingivae/erythema/present
        or
            miscellaneous/'bad taste'/present
        or
            miscellaneous/halitosis/present
    then
        patient/diagnosis/'Chronic Periodontitis Simplex'
    with
        certainty(0.8).

rule19:
    if
            pocketing/depth/'>3mm & <5mm'
        and
            'x-ray findings'/'alveolar bone loss'/present
        and
            tooth/'TTP'/positive
        and
            tooth/'gingival recession'/present
        and
            tooth/mobility/present
        and
            pain/site/tooth
        or
            pain/site/gingivae
        or
            tooth/migration/present
        or
            gingivae/bleeding/present
        or
            gingivae/swelling/present
        or
            gingivae/erythema/present
        or
            miscellaneous/'bad taste'/present
        or
            miscellaneous/halitosis/present
    then
        patient/diagnosis/'Chronic Periodontitis Simplex'
    with
        certainty(0.9).

rule20:
    if
            miscellaneous/age/'14 - 22'
        and
            pain/site/gingivae
        and
            gingivae/swelling/present
        and
            gingivae/erythema/present
        and
            gingivae/ulceration/present
    then
        patient/diagnosis/'Acute Ulcerative Gingivitis'
    with
        certainty(0.7).

rule21:
    if
            miscellaneous/age/'14 - 22'
        and
            pain/site/gingivae
        and
            gingivae/swelling/present
        and
            gingivae/erythema/present
        and
            gingivae/ulceration/present
        and
            gingivae/bleeding/present
        or
            miscellaneous/'bad taste'/present
        or
            miscellaneous/'local lymphadenopathy'/present
        or
            miscellaneous/pyrexia/present
        or
            miscellaneous/'general malaise'/present
        or
            miscellaneous/halitosis/present
    then
        patient/diagnosis/'Acute Ulcerative Gingivitis'
    with
        certainty(0.9).

rule22:
    if
            pocketing/depth/'>5mm'
        and
            tooth/mobility/present
        and
            tooth/migration/present
        and
            'x-ray findings'/'alveolar bone loss'/present
    then
        diagnosis/group/'Chronic Periodontitis Complex'
    with
        certainty(0.8).

rule23:
    if
            pocketing/depth/'>5mm'
        and
            tooth/mobility/present
        and
            tooth/migration/present
        and
            'x-ray findings'/'alveolar bone loss'/present
        and
            gingivae/swelling/present
        or
            gingivae/erythema/present
        or
            gingivae/bleeding/present
    then
        diagnosis/group/'Chronic Periodontitis Complex'
    with
        certainty(0.9).

rule24:
    if
            diagnosis/group/'Chronic Periodontitis Complex'
        and
            miscellaneous/age/'<14'
    then
        patient/diagnosis/'Prepubertal Chronic Periodontitis'
    with
        certainty(0.8).

rule25:
    if
            diagnosis/group/'Chronic Periodontitis Complex'
        and
            miscellaneous/age/'14 - 22'
    then
        patient/diagnosis/'Juvenile Chronic Periodontitis'
    with
        certainty(0.8).

rule26:
    if
            diagnosis/group/'Chronic Periodontitis Complex'
        and
            miscellaneous/age/'>22'
    then
        patient/diagnosis/'Post-juvenile Chronic Periodontitis'
    with
        certainty(0.8).

/**************************************************************************
 
    questionX:
    One question is provided for each O/A/V triple
 
 **************************************************************************/

question1:
    pain/localisation/[poor,accurate] 
    - "The localisation of the pain is... "
    - opposite.

question2:
    pain/site/[tooth,gingivae] 
    - "The site of the pain is... "
    - multiple.

question3:
    pain/character/['spontaneous onset','sudden onset',
    'intense & throbbing','severe & stabbing',dull] 
    - "The character of the pain is... "
    - multiple.

question5:
    pain/duration/[constant,intermittent] 
    - "The duration of the pain is... "
    - opposite.

question6:
    pain/'initiated by'/['clenching, biting or chewing','gentle touch',
                  cold,heat,sweet]
    - "Do any of these trigger or worsen the pain?"
    - multiple.

question7:
    pain/'relieved by'/[analgesics,'application of cold']
    - "Do any of these relieve the pain?"
    - multiple.

question8:
    tooth/'carious cavity'/[present,absent]
    - "Carious cavities are..."
    - opposite.

question9:
    tooth/mobility/[present,absent]
    - "Significant tooth mobility is..."
    - opposite.

question10:
    tooth/migration/[present,absent]
    - "Significant tooth migration is..."
    - opposite.

question11:
    tooth/'gingival recession'/[present,absent]
    - "Significant gingival recession is..."
    - opposite.

question12:
    tooth/'TTP'/[positive,negative]
    - "The tooth/teeth are TTP..."
    - opposite.

question13:
    tooth/'pulp test'/[positive,negative]
    - "A pulp test was..."
    - opposite.

question14:
    tooth/'recent fillings'/[present,absent]
    - "Evidence of recent fillings is..."
    - opposite.

question15:
    tooth/'polished high-spot on filling'/[present,absent]
    - "Evidence of a polished high-spot on a filling is..."
    - opposite.

question16:
    swelling/site/['over root apex',other]
    - "The site of the swelling is..."
    - opposite.

question17:
    pocketing/depth/['<3mm','>3mm & <5mm','>5mm']
    - "The depth of the pocketing is..."
    - opposite.

question18:
    gingivae/swelling/[present,absent]
    - "Gingival swelling is..."
    - opposite.

question19:
    gingivae/erythema/[present,absent]
    - "Gingival erythema is..."
    - opposite.

question20:
    gingivae/bleeding/[present,absent]
    - "Gingival bleeding is..."
    - opposite.

question21:
    gingivae/ulceration/[present,absent]
    - "Gingival ulceration is..."
    - opposite.

question22:
    'x-ray findings'/'alveolar bone loss'/[present,absent]
    - "Evidence from x-rays of alveolar bone loss is..."
    - opposite.

question23:
    miscellaneous/sinus/[present,absent]
    - "A sinus is..."
    - opposite.

question24:
    miscellaneous/'bad taste'/[present,absent]
    - "A bad taste is..."
    - opposite.

question25:
    miscellaneous/halitosis/[present,absent]
    - "Halitosis is..."
    - opposite.

question26:
    miscellaneous/trismus/[present,absent]
    - "Trismus is..."
    - opposite.

question27:
    miscellaneous/'local lymphadenopathy'/[present,absent]
    - "Local lymphadenopathy is.."
    - opposite.

question28:
    miscellaneous/age/['<14','14 - 22','>22']
    - "The age of the patient lies in which of these ranges..."
    - single.

question29:
    miscellaneous/'previous history'/[pulpitis,'Acute Ulcerative Gingivitis']
    - "Has the patient suffered from any the following in the past..."
    - multiple.

question30:
    miscellaneous/pyrexia/[present,absent]
    - "Does the patient have an elevated temperature? (pyrexia)... "
    - opposite.

question31:
    miscellaneous/'general malaise'/[present,absent]
    - "Is the patient suffering from a general malaise? "
    - opposite.

/**************************************************************************
 
    top_level_goals:
    These are the goals/diagnoses that the system tries to deduce
 
 **************************************************************************/

top_level_goals:
    patient -
    diagnosis -
        [
        'Acute Pulpitis',
        'Chronic Pulpitis',
        'Pericoronitis',
        'Acute Periapical Abscess',
        'Acute Lateral Abscess',
        'Chronic Marginal Gingivitis',
        'Acute Ulcerative Gingivitis',
        'Chronic Periodontitis Simplex',
        'Prepubertal Chronic Periodontitis',
        'Juvenile Chronic Periodontitis',
        'Post-juvenile Chronic Periodontitis'
        ].

/**************************************************************************/
/***************** END OF RULE_BASE: dental *******************************/
/**************************************************************************/
