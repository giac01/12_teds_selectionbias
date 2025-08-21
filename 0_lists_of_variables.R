# Final version -----------------------------------------------------------------

rq5z = c(
  
  ## Problematic variables that are commended out temporarily but we might want to add back!!

  "cohort",
  # "aonsby",

  # "pollution1998pca",
  
  
  ## First Contact Variables -------------------------------------------------
  
  # Family Structure
  "amumagetw",    # Maternal age at twin birth
  # "adadagetw",    # Paternal age at twin birth
  "aadults",      # Number of adults in household
  # "asingle",        # derived froma adults (1 = single parent, 0 = other, NA = missing partner details)
  
  # Zygosity 

  # Maternal Medical Risk Factors
  # "amedtot",      # Maternal medical risk factors total
  
  # Parental Socioeconomic Variables
  # "afaclas",      # Father's social class
  # "afajob",       # Father's job
  # "afasoc",       # Father's socioeconomic status
  # "afaspq",       # Father's socioeconomic position qualifications
  # "afawork",      # Father's work status
  # "afahqual",     # Father's highest qualification
  # "amoclas",      # Mother's social class                                     - mostly missing data
  # "amojob",       # Mother's job
  "amosoc2",
  # "amosoc",       # Mother's socioeconomic status                             - mostly missing data
  # "amospq",       # Mother's socioeconomic position qualifications
  # "amowork",      # Mother's work status                                      - removing this as rare cases of apprentices and foreman seem to lead to errors!
  "amohqualn",     # Mother's highest qualification (numeric)
  
  # Twin Medical Risk Factors (twin 1 only)
  # "atwmed1",      # Twin 1 medical risk factors
  # "atwmed2",
  
  # Environment and Demographics
  # "aethnic",      # Ethnicity of twins
  "aethnic",     # Ethnicity of twins (categorical)
  "alang",        # Main language spoken at home
  "anoldsibn",     # Number of older siblings
  "anyngsibn",     # Number of younger siblings
  "atwclub",      # Membership in Twins Club
  # "alookels",     # Twins looked after by anyone else
  
  # Postcode Linked Data
  "cens01pop98density",  # Population density                                   
  

  # Twin Health (twin 1 only)
  # "agenpro1",     # Twin 1 has genetic problems
  
  # Maternal Pregnancy Variables                                                - removed as they are captured in amedtot
  "asmoke",       # Maternal smoking during pregnancy
  # "adrink",       # Maternal drinking during pregnancy
  # "astress"      # Maternal stress during pregnancy
  
  ## Year 4 Variables ------------------------------------------
  
  # PARCA test totals
  # "ddrawt1",      # Drawing test
  # "ddrawt2",      # Drawing test
  # "dmant1",       # Manipulation test
  # "dmant2",       # Manipulation test
  # "doddt1",       # Odd-one-out test
  # "doddt2",       # Odd-one-out test
  # "dpuzt1",       # Puzzle test
  # "dpuzt2",       # Puzzle test
  # 
  # # PARCA totals and composites
  # # "dreparc1",     # Parent-reported PARCA
  # # "dreparc2",     # Parent-reported PARCA
  # # "dadparc1",     # Parent-administered PARCA total
  # # "dadparc2",     # Parent-administered PARCA total
  # # "dadparn1",     # Parent-administered non-verbal
  # # "dadparn2",     # Parent-administered non-verbal
  # # "dparca1",      # PARCA overall
  # # "dparca2",      # PARCA overall
  # 
  # # Language scales
  # "dpictot1",     # Picture vocabulary
  # "dpictot2",     # Picture vocabulary
  # "dvocab1",      # Vocabulary
  # "dvocab2",      # Vocabulary
  # "dtvoc1",       # Total vocabulary
  # "dtvoc2",       # Total vocabulary
  # "dgramma1",     # Grammar
  # "dgramma2",     # Grammar
  
  # Low language flags
  # "dllang1",      # Low language
  # "dllang2",      # Low language
  # "dllpic1",      # Low picture vocabulary
  # "dllpic2",      # Low picture vocabulary
  # "dllslow1",     # Slow language development
  # "dllslow2",     # Slow language development
  # "dlltalk1",     # Delayed talking
  # "dlltalk2",     # Delayed talking
  # "dllvoc1",      # Low vocabulary
  # "dllvoc2",      # Low vocabulary
  
  # General cognitive ability
  # "drawg1",       # General cognitive ability
  # "drawg2",       # General cognitive ability
  "dscnv1",       # Standardized non-verbal cognitive
  "dscnv2",       # Standardized non-verbal cognitive
  "dscv1",        # Standardized verbal cognitive
  "dscv2",        # Standardized verbal cognitive
  
  # SDQ scales (Year 4)
  # "dsdqbeht1",    # Total behavior problems
  # "dsdqbeht2",    # Total behavior problems
  "dsdqcont1",    # Conduct
  "dsdqcont2",    # Conduct
  # "dsdqemot1",    # Emotion
  # "dsdqemot2",    # Emotion
  "dsdqhypt1",    # Hyperactivity
  "dsdqhypt2",    # Hyperactivity
  # "dsdqpert1",    # Peer problems
  # "dsdqpert2",    # Peer problems
  # "dsdqprot1",    # Prosocial
  # "dsdqprot2",    # Prosocial
  
  # Anxiety measures
  "danxt1",       # ARBQ Anxiety-Related Behaviors
  "danxt2",       # ARBQ Anxiety-Related Behaviors
  
  # Household CHAOS (Year 4)
  "dchatot",      # Overall household chaos scale
  
  ## Year 7 Variables ------------------------------------------
  
  # Reading test
  # "gtowt1",       # TOWRE reading test composite
  # "gtowt2",       # TOWRE reading test composite
  # 
  # # Cognitive test scores
  # "gcongrt1",     # Cognitive grammar
  # "gcongrt2",     # Cognitive grammar
  # "gsimilt1",     # Similarities
  # "gsimilt2",     # Similarities
  # "gvocabt1",     # Vocabulary
  # "gvocabt2",     # Vocabulary
  # "gpicomt1",     # Picture completion
  # "gpicomt2",     # Picture completion
  # "gnwrt1",       # Non-word reading
  # "gnwrt2",       # Non-word reading
  # 
  # # Cognitive composites
  # "gcg1",         # General cognitive ability
  # "gcg2",         # General cognitive ability
  "gcl1",         # Verbal ability
  "gcl2",         # Verbal ability
  "gcn1",         # Non-verbal ability
  "gcn2",         # Non-verbal ability
  
  # Academic achievement scales
  "gt2ac1",       # 2-subject mean levels
  "gt2ac2",       # 2-subject mean levels
  # "gtmat1",       # Mathematics
  # "gtmat2",       # Mathematics
  # "gteng1",       # English
  # "gteng2",       # English
  
  # SDQ scales (Year 7)
  # "gpsdqprot1",   # Prosocial
  # "gpsdqprot2",   # Prosocial
  "gpsdqhypt1",   # Hyperactivity
  "gpsdqhypt2",   # Hyperactivity
  "gpsdqcont1",   # Conduct
  "gpsdqcont2",   # Conduct
  # "gpsdqpert1",   # Peer problems
  # "gpsdqpert2",   # Peer problems
  # "gpsdqemot1",   # Emotion
  # "gpsdqemot2",   # Emotion
  # "gpsdqbeht1",   # Total behavior problems
  # "gpsdqbeht2",   # Total behavior problems
  
  # Anxiety measures
  "gpanxt1",      # ARBQ Anxiety-Related Behaviors
  "gpanxt2",      # ARBQ Anxiety-Related Behaviors
  
  
  ## Year 8 Variables ---------------------------------------------------------
  
  "hconnt1",     # Conners ADHD overall Total at 8 (0-54)
  "hconnt2",     # Conners ADHD overall Total at 8 (0-54)
  
  
  # # Conners ADHD measures - MISSING                                           - MISSING SUBSCALES
  # "hconhit1",       # Conners Hyperactivity-Impulsivity scale.
  # "hconhit2",       # Conners Hyperactivity-Impulsivity scale.
  # 
  # "hconint1",      # Conners Inattention scale.
  # "hconint2",      # Conners Inattention scale.
  
  ## Year 9 Variables ---------------------------------------------------------
  
  # Cognitive composites
  # "icg1",         # General cognitive ability                                 - not needed in imputation as colinear with verbal and non-verbal ability? 
  # "icg2",         # General cognitive ability
  "icvb1",        # Verbal ability
  "icvb2",        # Verbal ability
  "icnv1",        # Non-verbal ability
  "icnv2",        # Non-verbal ability
  
  # Academic achievement scales
  # "iteng1",       # English
  # "iteng2",       # English
  # "itmat1",       # Mathematics
  # "itmat2",       # Mathematics
  # "itsci1",       # Science
  # "itsci2",       # Science
  # "it2ac1",       # 2-subject mean levels
  "it3ac1",       # 3-subject mean levels
  
  # Twin-reported SDQ
  "icsdqcont1",   # Conduct
  "icsdqcont2",   # Conduct
  # "icsdqemot1",   # Emotion
  # "icsdqemot2",   # Emotion
  "icsdqhypt1",   # Hyperactivity
  "icsdqhypt2",   # Hyperactivity
  # "icsdqpert1",   # Peer problems
  # "icsdqpert2",   # Peer problems
  # "icsdqprot1",   # Prosocial
  # "icsdqprot2",   # Prosocial
  # 
  # Parent-reported SDQ
  # "ipsdqprot1",   # Prosocial
  # "ipsdqprot2",   # Prosocial
  "ipsdqhypt1",   # Hyperactivity
  "ipsdqhypt2",   # Hyperactivity
  "ipsdqcont1",   # Conduct
  "ipsdqcont2",   # Conduct
  # "ipsdqpert1",   # Peer problems
  # "ipsdqpert2",   # Peer problems
  # "ipsdqemot1",   # Emotion
  # "ipsdqemot2",   # Emotion
  # "ipsdqbeht1",   # Total behavior problems
  
  # Parent Negative Feelings
  "icparnegt1",   # Parent negative feelings
  "icparnegt2",   # Parent negative feelings
  
  # Anxiety measures
  "ipanxt1",      # ARBQ Anxiety-Related Behaviors
  "ipanxt2",      # ARBQ Anxiety-Related Behaviors
  
  # Household CHAOS
  "ipchatot",     # Parent-reported household chaos                             
  # "icchato1",     # Twin-reported household chaos
  # "icchato2",     # Twin-reported household chaos
  
  
  ## Year 10 Variables  -----------------------------------------
  
  # Web test total scores
  # "jartt1",       # Author recognition total
  # "jartt2",       # Author recognition total
  
  
  # Web test adjusted scores
  # "jpiatta1",     # PIAT adjusted
  # "jmatta1",      # Mathematics adjusted
  # "jravta1",      # Ravens adjusted
  # "jvocta1",      # Vocabulary adjusted
  # "jgenta1",      # General knowledge adjusted
  
  # Cognitive composites
  # "jcg1",         # General cognitive ability
  "jcnv1",        # Non-verbal ability
  "jcnv2",        # Non-verbal ability
  "jcvb1",        # Verbal ability
  "jcvb2",        # Verbal ability

  # 
  # Academic achievement scales
  # "jteng1",       # English
  # "jteng2",       # English
  # "jtmat1",       # Mathematics
  # "jtmat2",       # Mathematics
  # "jtsci1",       # Science
  # "jtsci2",       # Science
  # "jt2ac1",       # 2-subject mean levels
  "jt3ac1",       # 3-subject mean levels
  
  # Web test times
  # "jtottm1",      # Total time for test battery
  # "jtottm2",      # Total time for test battery
  
  
  ## Year 12 Variables ---------------------------------------------------------
  
  # Twin web test scores
  # "lartot1",      # Author Recognition test
  # "lartot2",      # Author Recognition test
  # 
  # "ltotot1",      # TOAL total score
  # "ltotot2",      # TOAL total score
  
  # "ltottm1",      # Total time to complete batteries
  # "ltottm2",      # Total time to complete batteries
  # "lestcon1",     # Estimated connection type
  # "lestcon2",     # Estimated connection type
  
  # TOWRE test composite
  # "ltowt1",       # TOWRE reading test
  # "ltowt2",       # TOWRE reading test
  
  # Cognitive ability composites 
  # "lcg1",         # General cognitive ability
  # "lcg2",         # General cognitive ability
  # "lcvb1",        # Verbal ability                                            - commenting these out reduces errors! 
  # "lcvb2",        # Verbal ability
  # "lcnv1",        # Non-verbal ability
  # "lcnv2",        # Non-verbal ability
  
  # Academic achievement composites
  "lp3ac1",       # 3-subject mean (parent-reported)
  "lp3ac2",       # 3-subject mean (parent-reported)
  # "lp2ac1",       # 2-subject mean (parent-reported)
  # "lp2ac2",       # 2-subject mean (parent-reported)
  # "lteng1",       # English (teacher-reported)
  # "lteng2",       # English (teacher-reported)
  # "ltmat1",       # Mathematics (teacher-reported)
  # "ltmat2",       # Mathematics (teacher-reported)
  # "ltsci1",       # Science (teacher-reported)
  # "ltsci2",       # Science (teacher-reported)
  # "lt3ac1",       # 3-subject mean (teacher-reported)
  # "lt3ac2",       # 3-subject mean (teacher-reported)
  # "lt2ac1",       # 2-subject mean (teacher-reported)
  # "lt2ac2",       # 2-subject mean (teacher-reported)
  
  # Twin-reported SDQ
  # "lcsdqprot1",   # Prosocial
  # "lcsdqprot2",   # Prosocial
  "lcsdqhypt1",   # Hyperactivity
  "lcsdqhypt2",   # Hyperactivity
  "lcsdqcont1",   # Conduct
  "lcsdqcont2",   # Conduct
  # "lcsdqpert1",   # Peer problems
  # "lcsdqpert2",   # Peer problems
  # "lcsdqemot1",   # Emotion
  # "lcsdqemot2",   # Emotion
  # 
  # Parent-reported SDQ
  # "lpsdqprot1",   # Prosocial
  # "lpsdqprot2",   # Prosocial
  "lpsdqhypt1",   # Hyperactivity
  "lpsdqhypt2",   # Hyperactivity
  "lpsdqcont1",   # Conduct
  "lpsdqcont2",   # Conduct
  # "lpsdqpert1",   # Peer problems
  # "lpsdqpert2",   # Peer problems
  # "lpsdqemot1",   # Emotion
  # "lpsdqemot2",   # Emotion
  # "lpsdqbeht1",   # Total behavior problems
  # "lpsdqbeht2",   # Total behavior problems
  
  # Mood & Feelings Questionnaire 
  # nmote that enhanced versions include sdq measures in them. 
  "lcmfqt1",      # MFQ - twin report (plain version)
  "lcmfqt2",      # MFQ - twin report (plain version)
  # "lcemfqt1",     # MFQ - twin report (enhanced version)
  # "lcemfqt2",     # MFQ - twin report (enhanced version)
  "lpmfqt1",      # MFQ - parent report (plain version)
  "lpmfqt2",      # MFQ - parent report (plain version)
  # "lpemfqt1",     # MFQ - parent report (enhanced version)
  # "lpemfqt2",     # MFQ - parent report (enhanced version)
  # 
  # Conners ADHD measures
  "lpconnt1",     # Conners ADHD - parent report
  "lpconnt2",     # Conners ADHD - parent report
  
  # Parent Negative Feelings
  "lcparnegt1",   # Parent negative feelings
  "lcparnegt2",   # Parent negative feelings
  
  # Household CHAOS
  "lpchatot",     # Parent-reported household chaos                             
  "lcchato1",     # Twin-reported household chaos
  "lcchato2",      # Twin-reported household chaos
  
  ## Year 14 Variables -----------------------------------------
  
  # Twin web tests adjusted scores
  # "nrvtota1",     # Ravens adjusted
  # "nrvtota2",     # Ravens adjusted
  # "nsctota1",     # Adjusted total score for the Science web test
  # "nsctota2",     # Adjusted total score for the Science web test
  # "nvctota1",     # Vocabulary adjusted
  # "nvctota2",     # Vocabulary adjusted
  
  # Total time to complete the battery
  # "ntottm1",      # Total completion time
  # "ntottm2",      # Total completion time
  # 
  # Cognitive composite
  # "ncg1",         # General cognitive ability
  # "ncg2",         # General cognitive ability
  
  # Achievement composites (Academic achievement scales, derived from end-of-KS3 ratings reported to families then reported by parents to TEDS in the SLQ.)
  "npks3t3a1",    # 3-subject (KS3 ratings, all cohorts)
  "npks3t3a2",    # 3-subject (KS3 ratings, all cohorts)

  # Conners ADHD measures - twin reported
  "ncconint1",    # Conners ADHD inattention
  "ncconint2",    # Conners ADHD inattention
  "ncconhit1",    # Conners ADHD hyperactivity/impulsivity
  "ncconhit2",    # Conners ADHD hyperactivity/impulsivity
  
  # Conners ADHD measures - parent reported
  "npconnt1",     # Conners ADHD - parent report (COULD REPLACE WITH SEPERATE INATTENTION ADN HYPERACTIVITY SCALES IF REQUSTED FOR THAT DATA)
  "npconnt2",     # Conners ADHD - parent report (COULD REPLACE WITH SEPERATE INATTENTION ADN HYPERACTIVITY SCALES IF REQUSTED FOR THAT DATA)
  
  # Parent Negative Feelings
  "ncparnegt1",   # Parent negative feelings
  "ncparnegt2",   # Parent negative feelings
  
  # Household CHAOS
  # "npchatot",     # Parent-reported household chaos                           - MISSING
  "ncchato1",     # Twin-reported household chaos
  "ncchato2",      # Twin-reported household chaos
  
  ## Year 16 Variables -----------------------------------------
  
  # Twin web activities
  # "pcrvtota1",    # Ravens adjusted
  # "pcrvtota2",    # Ravens adjusted
  # "pcuntota1",    # Understanding adjusted
  # "pcuntota2",    # Understanding adjusted
  # "pcvctota1",    # Vocabulary adjusted
  # "pcvctota2",    # Vocabulary adjusted
  # 
  # # Weber fraction scores for Dot Task
  # "pcnswebcor1",  # Weber fraction scores - corrected
  # "pcnswebcor2",  # Weber fraction scores - corrected
  # "pcnswebunc1",  # Weber fraction scores - uncorrected
  # "pcnswebunc2",  # Weber fraction scores - uncorrected
  
  # Total time taken for web questionnaires
  # "pcqattt1",     # Questionnaire A total time
  # "pcqattt2",     # Questionnaire A total time
  # "pcqbttt1",     # Questionnaire B total time
  # "pcqbttt2",     # Questionnaire B total time
  # "pcqcttt1",     # Questionnaire C total time
  # "pcqcttt2",     # Questionnaire C total time
  # "pcqdttt1",     # Questionnaire D total time
  # "pcqdttt2",     # Questionnaire D total time
  
  # Cognitive composite
  # "pcg1",         # General cognitive ability
  # "pcg2",         # General cognitive ability
  
  # GCSE results
  # "pcexgcsecoregrdm1",  # Mean grades for GCSEs
  # "pcexgcsecoregrdm2",  # Mean grades for GCSEs
  
  # Mood & Feelings Questionnaire
  # "pcbhmfqt1",     # MFQ - twin report - already in the main set of variables
  # "pcbhmfqt2",     # MFQ - twin report - already in the main set of variables
  # "ppbhmfqt1",    # MFQ - parent report
  # "ppbhmfqt2",    # MFQ - parent report                                       - ONLY TWIN 2 is missing for some reason!!!
  
  # Twin-reported SDQ
  # "pcbhsdqemot1", # Emotion
  # "pcbhsdqemot2", # Emotion
  "pcbhsdqcont1", # Conduct
  "pcbhsdqcont2", # Conduct
  "pcbhsdqhypt1", # Hyperactivity
  "pcbhsdqhypt2", # Hyperactivity
  # "pcbhsdqpert1", # Peer problems
  # "pcbhsdqpert2", # Peer problems
  # "pcbhsdqprot1", # Prosocial
  # "pcbhsdqprot2", # Prosocial
  # 
  # Parent-reported SDQ
  # "ppbhsdqbeht1", # Total behavior problems
  # "ppbhsdqbeht2", # Total behavior problems
  "ppbhsdqcont1", # Conduct
  "ppbhsdqcont2", # Conduct
  "ppbhsdqhypt1", # Hyperactivity
  "ppbhsdqhypt2", # Hyperactivity
  # "ppbhsdqprot1", # Prosocial
  # "ppbhsdqprot2", # Prosocial
  
  # ARBQ Anxiety-Related Behaviors
  "ppbhanxt1",    # Anxiety - parent report
  "ppbhanxt2",    # Anxiety - parent report
  
  # Conners ADHD measures
  # "ppbhconnt1",   # Conners ADHD - parent report
  # "ppbhconnt2",   # Conners ADHD - parent report                                - ONLY TWIN 2 missing!!
  
  # Household CHAOS
  "pcchatot1",     # Twin-reported household chaos
  "pcchatot2",     # Twin-reported household chaos
  
  ## Year 21 variables ----------------------------------------------------
  
  # Twin-reported SDQ
  # "u1csdqemot1", # Emotion
  # "u1csdqemot2", # Emotion
  "u1csdqcont1", # Conduct
  "u1csdqcont2", # Conduct
  "u1csdqhypt1", # Hyperactivity
  "u1csdqhypt2", # Hyperactivity
  # "u1csdqpert1", # Peer problems
  # "u1csdqpert2", # Peer problems
  # "u1csdqprot1", # Prosocial
  # "u1csdqprot2", # Prosocial
  
  ## Year 26 variables ----------------------------------------------------
  
  # Twin-reported SDQ
  # "zmhsdqemot1", # Emotion
  # "zmhsdqemot2", # Emotion
  "zmhsdqcont1", # Conduct
  "zmhsdqcont2", # Conduct
  "zmhsdqhypt1", # Hyperactivity
  "zmhsdqhypt2" # Hyperactivity
  # "zmhsdqpert1", # Peer problems
  # "zmhsdqpert2", # Peer problems
  # "zmhsdqprot1", # Prosocial
  # "zmhsdqprot2" # Prosocial
  
  
)


if (FALSE){

  iwalk(rq5z, ~ {
    tbl <- table(df[[.x]], useNA = "always")
    if (length(tbl) <= 10) {
      cat("\n", .x, ":\n")
      print(tbl)
    }
  })
    
}
