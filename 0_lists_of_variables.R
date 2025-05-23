rq5z = c(
  # ==========================================
  # First Contact Variables
  # ==========================================
  
  # Family Structure
  "amumagetw",    # Maternal age at twin birth
  "adadagetw",    # Paternal age at twin birth
  "aadults",      # Number of adults in household
  
  # Zygosity 
  # "aalgzyg",      # Estimated zygosity
  
  # Maternal Medical Risk Factors
  "amedtot",      # Maternal medical risk factors total
  
  # Parental Socioeconomic Variables
  "afaclas",      # Father's social class
  "afajob",       # Father's job
  "afasoc",       # Father's socioeconomic status
  "afaspq",       # Father's socioeconomic position qualifications
  "afawork",      # Father's work status
  "afahqual",     # Father's highest qualification
  "amoclas",      # Mother's social class
  "amojob",       # Mother's job
  "amosoc",       # Mother's socioeconomic status
  "amospq",       # Mother's socioeconomic position qualifications
  "amowork",      # Mother's work status
  "amohqual",     # Mother's highest qualification
  
  # Twin Medical Risk Factors (twin 1 only)
  "atwmed1",      # Twin 1 medical risk factors
  
  # Environment and Demographics
  # "aethnic",      # Ethnicity of twins
  "aethnicc",     # Ethnicity of twins (categorical)
  "alang",        # Main language spoken at home
  "anoldsib",     # Number of older siblings
  "anyngsib",     # Number of younger siblings
  "atwclub",      # Membership in Twins Club
  "alookels",     # Twins looked after by anyone else
  
  # Postcode Linked Data
  "cens01pop98density",  # Population density                                   - MISSING
  
  # Neighborhood pollution variables (linked to 2001 data)
  "pollutionYYYYpm10",   # PM10 pollution levels                                - ALL POLLUTION VARIABLES ARE MISSING!
  "pollutionYYYYpm25",   # PM2.5 pollution levels
  "pollutionYYYYno2",    # NO2 pollution levels
  "pollutionYYYYnox",    # NOx pollution levels
  "pollutionYYYYbenzene",# Benzene pollution levels
  "pollutionYYYYozone",  # Ozone pollution levels
  
  # Twin Health (twin 1 only)
  "agenpro1",     # Twin 1 has genetic problems
  
  # Maternal Pregnancy Variables
  "asmoke",       # Maternal smoking during pregnancy
  "adrink",       # Maternal drinking during pregnancy
  "astress",      # Maternal stress during pregnancy
  
  # ==========================================
  # Year 2 Variables (twin 1 only)
  # ==========================================
  
  # Cognitive measures
  "brawg1",       # General cognitive ability
  "badparn1",     # Parent-administered PARCA
  "breparc1",     # Parent-reported PARCA
  "bparca1",      # PARCA overall
  "bvocab1",      # Vocabulary
  "bgramma1",     # Grammar
  
  # Block test and composite scores
  "bblockt1",     # Block test total score
  "bcopyt1",      # Copy test total score
  "bdrawt1",      # Drawing test total score
  "bfoldt1",      # Folding test total score
  "bmatcht1",     # Matching test total score
  "badparc1",     # Parent-administered PARCA total
  "buse1",        # Word use
  "bcomplx1",     # Sentence complexity
  
  # Test item scores (Year 2)
  # "bpb01s1", "bpb02s1", "bpb03s1", "bpb04s1",  # Block test items
  # "bpc01s1", "bpc05s1", "bpc06s1", "bpc07s1",  # Copy test items
  # "bpm01s1", "bpm02s1", "bpm03s1", "bpm04s1",  # Matching test items part 1
  # "bpm05s1", "bpm06s1", "bpm07s1", "bpm08s1",  # Matching test items part 2
  # "bwu01s1", "bwu02s1", "bwu03s1",             # Word use items part 1
  # "bwu04s1", "bwu05s1", "bwu06s1",             # Word use items part 2
  
  # SDQ-comparable Behar subscales (Year 2)
  # "bsdqcbeht1",   # Total behaviour problems
  "bsdqccont1",   # Conduct subscale (0-8)
  "bsdqcemot1",   # Emotion subscale (0-4)
  "bsdqchypt1",   # Hyperactivity subscale (0-6)
  "bsdqcpert1",   # Peer problem subscale (0-6) 
  "bsdqcprot1",   # Prosocial subscale (0-10)
  
  # ==========================================
  # Year 3 Variables (twin 1 only)
  # ==========================================
  
  # PARCA test totals
  "coddt1",       # Odd-one-out test
  "cdrawt1",      # Drawing test 
  "cmatcht1",     # Matching test
  
  # PARCA totals and composites
  "creparc1",     # Parent-reported PARCA
  "cadparc1",     # Parent-administered PARCA
  "cadparn1",     # Parent-administered non-verbal
  "cparca1",      # PARCA overall
  
  # Language scores
  "cvocab1",      # Vocabulary
  "ctvoc1",       # Total vocabulary
  "cuse1",        # Word use
  "ccomplx1",     # Sentence complexity
  "cgramma1",     # Grammar
  
  # General cognitive ability
  "crawg1",       # General cognitive ability
  "cscnv1",       # Standardized non-verbal cognitive
  "cscv1",        # Standardized verbal cognitive
  
  # SDQ-comparable scales (Year 3)
  "csdqccont1",   # Conduct
  "csdqcemot1",   # Emotion
  "csdqchypt1",   # Hyperactivity
  "csdqcpert1",   # Peer problems
  "csdqcprot1",   # Prosocial
  # "csdqcbeht1",   # Total behavior problems
  
  # Anxiety measures
  "canxt1",       # ARBQ Anxiety-Related Behaviors
  
  # Household CHAOS (Year 3)
  "cchatot",      # Overall household chaos scale
  
  # ==========================================
  # Year 4 Variables (twin 1 only)
  # ==========================================
  
  # PARCA test totals
  "ddrawt1",      # Drawing test
  "dmant1",       # Manipulation test
  "doddt1",       # Odd-one-out test
  "dpuzt1",       # Puzzle test
  
  # PARCA totals and composites
  # "dreparc1",     # Parent-reported PARCA
  # "dadparc1",     # Parent-administered PARCA total
  # "dadparn1",     # Parent-administered non-verbal
  # "dparca1",      # PARCA overall
  
  # Language scales
  "dpictot1",     # Picture vocabulary
  "dvocab1",      # Vocabulary
  "dtvoc1",       # Total vocabulary
  "dgramma1",     # Grammar
  
  # Low language flags
  # "dllang1",      # Low language
  # "dllpic1",      # Low picture vocabulary
  # "dllslow1",     # Slow language development
  # "dlltalk1",     # Delayed talking
  # "dllvoc1",      # Low vocabulary
  
  # General cognitive ability
  "drawg1",       # General cognitive ability
  "dscnv1",       # Standardized non-verbal cognitive
  "dscv1",        # Standardized verbal cognitive
  
  # SDQ scales (Year 4)
  # "dsdqbeht1",    # Total behavior problems
  "dsdqcont1",    # Conduct
  "dsdqemot1",    # Emotion
  "dsdqhypt1",    # Hyperactivity
  "dsdqpert1",    # Peer problems
  "dsdqprot1",    # Prosocial
  
  # Anxiety measures
  "danxt1",       # ARBQ Anxiety-Related Behaviors
  
  # Household CHAOS (Year 4)
  "dchatot",      # Overall household chaos scale
  
  # ==========================================
  # Year 7 Variables (twin 1 only)
  # ==========================================
  
  # Reading test
  "gtowt1",       # TOWRE reading test composite
  
  # Cognitive test scores
  "gcongrt1",     # Cognitive grammar
  "gsimilt1",     # Similarities
  "gvocabt1",     # Vocabulary
  "gpicomt1",     # Picture completion
  "gnwrt1",       # Non-word reading
  
  # Cognitive composites
  "gcg1",         # General cognitive ability
  "gcl1",         # Verbal ability
  "gcn1",         # Non-verbal ability
  
  # Academic achievement scales
  "gt2ac1",       # 2-subject mean levels
  "gtmat1",       # Mathematics
  "gteng1",       # English
  
  # SDQ scales (Year 7)
  "gpsdqprot1",   # Prosocial
  "gpsdqhypt1",   # Hyperactivity
  "gpsdqcont1",   # Conduct
  "gpsdqpert1",   # Peer problems
  "gpsdqemot1",   # Emotion
  # "gpsdqbeht1",   # Total behavior problems
  
  # Anxiety measures
  "gpanxt1",      # ARBQ Anxiety-Related Behaviors
  
  # ==========================================
  # Year 8 Variables
  # ==========================================
  
  # Conners ADHD measures
  "hconnt1",      # Conners ADHD composite
  
  # ==========================================
  # Year 9 Variables (twin 1 only)
  # ==========================================
  
  # Twin tests total scores
  "icpuzt1",      # Puzzle
  "icwrdt1",      # Word
  "icshpt1",      # Shape
  "icgent1",      # General knowledge
  
  # Cognitive composites
  "icg1",         # General cognitive ability
  "icvb1",        # Verbal ability
  "icnv1",        # Non-verbal ability
  
  # Academic achievement scales
  "iteng1",       # English
  "itmat1",       # Mathematics
  "itsci1",       # Science
  # "it2ac1",       # 2-subject mean levels
  # "it3ac1",       # 3-subject mean levels
  
  # Twin-reported SDQ
  "icsdqcont1",   # Conduct
  "icsdqemot1",   # Emotion
  "icsdqhypt1",   # Hyperactivity
  "icsdqpert1",   # Peer problems
  "icsdqprot1",   # Prosocial
  
  # Parent-reported SDQ
  "ipsdqprot1",   # Prosocial
  "ipsdqhypt1",   # Hyperactivity
  "ipsdqcont1",   # Conduct
  "ipsdqpert1",   # Peer problems
  "ipsdqemot1",   # Emotion
  # "ipsdqbeht1",   # Total behavior problems
  
  # Parent Negative Feelings
  "icparnegt1",   # Parent negative feelings
  
  # Anxiety measures
  "ipanxt1",      # ARBQ Anxiety-Related Behaviors
  
  # Household CHAOS
  "ipchatot",     # Parent-reported household chaos                             -MISSING
  "icchato1",     # Twin-reported household chaos
  
  # ==========================================
  # Year 10 Variables (twin 1 only)
  # ==========================================
  
  # Web test total scores
  # "jpiatt1",      # PIAT
  # "jmatt1",       # Mathematics total
  # "jmcat1t1",     # Mathematics subtest 1
  # "jmcat2t1",     # Mathematics subtest 2
  # "jmcat3t1",     # Mathematics subtest 3
  # "jmcat4t1",     # Mathematics subtest 4
  # "jmcat5t1",     # Mathematics subtest 5
  # "jgent1",       # General knowledge
  # "jvoct1",       # Vocabulary
  "jpict1",       # Picture completion
  
  # Ravens tests
  # "jravt1",       # Ravens total
  # "jrcatat1",     # Ravens category A
  # "jrcatbt1",     # Ravens category B
  # "jrcatct1",     # Ravens category C
  # "jrcatdt1",     # Ravens category D
  # "jrcatet1",     # Ravens category E
  
  # Author recognition
  # "jarcort1",     # Author recognition correct
  # "jarinct1",     # Author recognition incorrect
  "jartt1",       # Author recognition total
  
  # Web test adjusted scores
  "jpiatta1",     # PIAT adjusted
  "jmatta1",      # Mathematics adjusted
  "jravta1",      # Ravens adjusted
  "jvocta1",      # Vocabulary adjusted
  "jgenta1",      # General knowledge adjusted
  
  # Cognitive composites
  "jcg1",         # General cognitive ability
  "jcnv1",        # Non-verbal ability
  "jcvb1",        # Verbal ability
  
  # Academic achievement scales
  "jteng1",       # English
  "jtmat1",       # Mathematics
  "jtsci1",       # Science
  # "jt2ac1",       # 2-subject mean levels
  # "jt3ac1",       # 3-subject mean levels
  
  # Web test times
  "jtottm1",      # Total time for test battery
  
  # ==========================================
  # Year 12 Variables (twin 1 only)
  # ==========================================
  
  # Twin web test scores
  "lartot1",      # Author Recognition test
  
  # Mathematics sub-test scores
  # "lma1tot1",     # Mathematics subtest 1
  # "lma2tot1",     # Mathematics subtest 2
  # "lma3tot1",     # Mathematics subtest 3
  
  # TOAL test scores
  # "ltobasal1",    # TOAL basal score
  # "ltoceiling1",  # TOAL ceiling score
  "ltotot1",      # TOAL total score
  
  # Test scores adjusted for discontinued items
  "lgktota1",     # General knowledge adjusted
  "lgotota1",     # Go/No-go adjusted
  "lhstota1",     # History adjusted
  "ljgtota1",     # Judgment adjusted
  "lmatota1",     # Mathematics adjusted
  "lpitota1",     # Picture adjusted
  "lrvtota1",     # Ravens adjusted
  "ltotota1",     # TOAL adjusted
  "lvctota1",     # Vocabulary adjusted                                         - MISSING
  
  # Twin web test times
  # "lbat1tm1",     # Battery 1 completion time
  # "lbat2tm1",     # Battery 2 completion time
  "ltottm1",      # Total time to complete batteries
  # "lestcon1",     # Estimated connection type
  
  # TOWRE test composite
  "ltowt1",       # TOWRE reading test
  
  # Cognitive ability composites
  "lcg1",         # General cognitive ability
  "lcvb1",        # Verbal ability
  "lcnv1",        # Non-verbal ability
  
  # Academic achievement composites
  "lp3ac1",       # 3-subject mean (parent-reported)
  "lp2ac1",       # 2-subject mean (parent-reported)
  "lteng1",       # English (teacher-reported)
  "ltmat1",       # Mathematics (teacher-reported)
  "ltsci1",       # Science (teacher-reported)
  "lt3ac1",       # 3-subject mean (teacher-reported)
  "lt2ac1",       # 2-subject mean (teacher-reported)
  
  # Twin-reported SDQ
  "lcsdqprot1",   # Prosocial
  "lcsdqhypt1",   # Hyperactivity
  "lcsdqcont1",   # Conduct
  "lcsdqpert1",   # Peer problems
  "lcsdqemot1",   # Emotion
  
  # Parent-reported SDQ
  "lpsdqprot1",   # Prosocial
  "lpsdqhypt1",   # Hyperactivity
  "lpsdqcont1",   # Conduct
  "lpsdqpert1",   # Peer problems
  "lpsdqemot1",   # Emotion
  # "lpsdqbeht1",   # Total behavior problems
  
  # Mood & Feelings Questionnaire
  "lcmfqt1",      # MFQ - twin report (plain version)
  "lcemfqt1",     # MFQ - twin report (enhanced version)
  "lpmfqt1",      # MFQ - parent report (plain version)
  "lpemfqt1",     # MFQ - parent report (enhanced version)
  
  # Conners ADHD measures
  "lpconnt1",     # Conners ADHD - parent report
  
  # Parent Negative Feelings
  "lcparnegt1",   # Parent negative feelings
  
  # Household CHAOS
  "lpchatot",     # Parent-reported household chaos                             - missing
  "lcchato1",     # Twin-reported household chaos
  
  # ==========================================
  # Year 14 Variables (twin 1 only)
  # ==========================================
  
  # Twin web tests adjusted scores
  "nrvtota1",     # Ravens adjusted
  "nsctota1",     # Sentence completion adjusted
  "nvctota1",     # Vocabulary adjusted
  
  # Mean test item answer time
  # "nrvatm1",      # Ravens average time per item
  # "nscatm1",      # Sentence completion average time
  # "nvcatm1",      # Vocabulary average time
  # 
  # Total time to complete the battery
  "ntottm1",      # Total completion time
  
  # Cognitive composite
  "ncg1",         # General cognitive ability
  
  # Achievement composites (Academic achievement scales, derived from end-of-KS3 ratings reported to families then reported by parents to TEDS in the SLQ.)
  # "nt2a1",        # 2-subject (teacher-rated, cohort 1)
  # "nt3a1",        # 3-subject (teacher-rated, cohort 1)
  # "npks3t2a1",    # 2-subject (KS3 ratings, all cohorts)
  "npks3t3a1",    # 3-subject (KS3 ratings, all cohorts)
  # "npks3tall1",   # All subjects (KS3 ratings, all cohorts)
  
  # Conners ADHD measures - twin reported
  # "ncconnt1",     # Conners ADHD overall
  "ncconint1",    # Conners ADHD inattention
  "ncconhit1",    # Conners ADHD hyperactivity/impulsivity
  
  # Conners ADHD measures - parent reported
  "npconnt1",     # Conners ADHD - parent report (COULD REPLACE WITH SEPERATE INATTENTION ADN HYPERACTIVITY SCALES IF REQUSTED FOR THAT DATA)
  
  # Parent Negative Feelings
  "ncparnegt1",   # Parent negative feelings
  
  # Household CHAOS
  "npchatot",     # Parent-reported household chaos
  "ncchato1",     # Twin-reported household chaos
  
  # ==========================================
  # Year 16 Variables (twin 1 only)
  # ==========================================
  
  # Twin web activities
  "pcrvtota1",    # Ravens adjusted
  "pcuntota1",    # Understanding adjusted
  "pcvctota1",    # Vocabulary adjusted
  
  # Weber fraction scores for Dot Task
  "pcnswebcor1",  # Weber fraction scores - corrected
  # "pcnswebunc1",  # Weber fraction scores - uncorrected
  
  # Total time taken for web questionnaires
  # "pcqattt1",     # Questionnaire A total time
  # "pcqbttt1",     # Questionnaire B total time
  # "pcqcttt1",     # Questionnaire C total time
  # "pcqdttt1",     # Questionnaire D total time
  
  # Cognitive composite
  "pcg1",         # General cognitive ability
  
  # GCSE results
  "pcexgcsecoregrdm1",  # Mean grades for GCSEs
  
  # Mood & Feelings Questionnaire
  "pcbhmfqt1",     # MFQ - twin report
  "ppbhmfqt1",    # MFQ - parent report
  
  # Twin-reported SDQ
  "pcbhsdqemot1", # Emotion
  "pcbhsdqcont1", # Conduct
  "pcbhsdqhypt1", # Hyperactivity
  "pcbhsdqpert1", # Peer problems
  "pcbhsdqprot1", # Prosocial
  
  # Parent-reported SDQ
  "ppbhsdqbeht1", # Total behavior problems
  "ppbhsdqcont1", # Conduct
  "ppbhsdqhypt1", # Hyperactivity
  "ppbhsdqprot1", # Prosocial
  
  # ARBQ Anxiety-Related Behaviors
  "ppbhanxt1",    # Anxiety - parent report
  
  # Conners ADHD measures
  "ppbhconnt1",   # Conners ADHD - parent report
  
  # Household CHAOS
  "pcchatot1",    # Twin-reported household chaos
  
  # ==========================================
  # Year 18 Variables (twin 1 only)
  # ==========================================
  
  # Web study battery status
  # "rcfdata1",     # Data available (FFMP)
  # "rcfstatus1",   # Status (FFMP)
  # "rcfnact1",     # Number of activities (FFMP)
  # 
  # "rcbdata1",     # Data available (Bricks)
  # "rcbstatus1",   # Status (Bricks)
  # "rcbnact1",     # Number of activities (Bricks)
  # "rckndat1",     # Completed activities (Bricks)
  # 
  # "rckdata1",     # Data available (Kings Challenge)
  # "rcknact1",     # Number of activities (Kings)
  # "rckstatus1",   # Status (Kings Challenge)
  # 
  # Bricks scales and scores
  "rcbspsabm1",     # SPAA spatial ability overall mean scale (twin Bricks web study), 1 to 5
  "rcbspgnxm1",     # SPAA general anxiety overall mean scale (twin Bricks web study), 1 to 4
  # "rcbspgnxm1",   # Spatial ability - general anxiety
  # "rcbspmnxm1",   # Spatial ability - mental rotation
  # "rcbspsabm1",   # Spatial ability - spatial ability battery
  # "rcbspsnxm1",   # Spatial ability - specialized 
  
  # Bricks subtest scores
  # "rcb2rtot1",    # 2D rotation total
  # "rcb2rvtot1",   # 2D rotation-visualization total
  # "rcb2vtot1",    # 2D visualization total
  # "rcb3rvtot1",   # 3D rotation-visualization total
  # "rcb3rtot1",    # 3D rotation total
  # "rcb3vtot1",    # 3D visualization total
  
  # Bricks overall test scores
  # "rcbrt1",       # Rotation total
  # "rcbrvt1",      # Rotation-visualization total
  # "rcbvt1",       # Visualization total
  # "rcb2dt1",      # 2D total
  # "rcb3dt1",      # 3D total
  # "rcbt1",        # Bricks overall total
  
  # Kings Challenge item responses
  "rckemtot1",     # Elithorn mazes total
  # "rckemtotp1",    # Elithorn mazes total points
  
  # 2D and 3D drawing items
  # "rck2d1an1",    # 2D drawing item 1
  # "rck2d2an1",    # 2D drawing item 2
  # "rck2d3an1",    # 2D drawing item 3
  # "rck2d4an1",    # 2D drawing item 4
  # "rck2d5an1",    # 2D drawing item 5
  # "rck3d1an1",    # 3D drawing item 1
  # "rck3d2an1",    # 3D drawing item 2
  # "rck3d3an1",    # 3D drawing item 3
  # "rck3d4an1",    # 3D drawing item 4
  # "rck3d5an1",    # 3D drawing item 5
  # "rck3d6an1",    # 3D drawing item 6
  # "rck3d7an1",    # 3D drawing item 7
  
  # Navigation total scores
  # "rcnas1",       # Navigation - abstract score
  # "rcnss1",       # Navigation - spatial score
  "rcnts1",       # Navigation - total score
  
  # UCAS scores (university admissions)
  "rcqucast1",     # Estimated UCAS total point score
  
  # ==========================================
  # Year 21 Variables
  # ==========================================
  
  # G-game cognitive test scores
  "ucgisttot1",    # G-game ISTO/NVRA/NVRB subtest total
  "ucgravtot1",    # G-game Ravens subtest total (0-11)
  "ucgvoctot1",    # G-game Vocabulary subtest total (0-8)
  "ucgmistot1",    # G-game Missing Letter subtest total (0-6)
  "ucgvertot1",    # G-game Verbal Reasoning subtest total (0-6)
  
  # Cognitive composites
  "ucgnvt1",      # Non-verbal ability 
  "ucgvbt1",      # Verbal ability
  "ucgt1",        # General cognitive ability
  
  # Mood & Feelings Questionnaire - age 21
  "u1cmfqt1",      # MFQ - twin report (phase 1)
  "ucv1mfqt1",     # MFQ - twin report (covid phase 1)
  
  # Twin-reported SDQ - age 21 phase 1
  "u1csdqemot1",  # Emotion 
  "u1csdqpert1",  # Peer problems
  "u1csdqhypt1",  # Hyperactivity
  "u1csdqcont1",  # Conduct
  "u1csdqprot1",  # Prosocial
  
  # Twin-reported SDQ - age 21 covid phase 1
  "ucv1sdqemot1", # Emotion
  "ucv1sdqpert1", # Peer problems
  "ucv1sdqhypt1", # Hyperactivity
  "ucv1sdqcont1", # Conduct
  "ucv1sdqprot1", # Prosocial
  
  # Parent-reported SDQ - age 21 phase 1
  "u1psdqemot1",  # Emotion
  "u1psdqpert1",  # Peer problems
  "u1psdqhypt1",  # Hyperactivity
  "u1psdqcont1",  # Conduct
  "u1psdqprot1",  # Prosocial
  # "u1psdqbeht1",  # Total behavior problems
  
  # GAD-D Anxiety Symptoms
  "u2cganxt1",     # Anxiety - twin report (phase 2)
  "ucv1ganxt1",    # Anxiety - twin report (covid phase 1)
  
  # Conners ADHD measures - age 21
  "u2cconnt1",    # Conners ADHD overall
  "u2cconninat1", # Conners ADHD inattention
  "u2cconnhypt1", # Conners ADHD hyperactivity/impulsivity
  "u1pcont1",     # Conners ADHD - parent report
  
  # Household CHAOS - age 21
  "u1cchaost1",   # Twin-reported household chaos
  
  # ==========================================
  # Year 26 Variables
  # ==========================================
  
  # Highest qualification
  "zmhhqual1",     # Highest level of qualification
  
  # Employment status variables
  "zmhempst1",     # Employment status
  "zmhneet1",      # Not in education, employment or training
  "zmhempinc1",    # Employment income
  
  # Mood & Feelings Questionnaire
  "zmhmfqt1",     # MFQ - twin report
  
  # SDQ scales
  "zmhsdqemot1",  # Emotion
  "zmhsdqpert1",  # Peer problems
  "zmhsdqhypt1",  # Hyperactivity
  "zmhsdqcont1",  # Conduct
  "zmhsdqprot1",  # Prosocial
  
  # GAD-D Anxiety Symptoms
  "zmhganxt1",     # Anxiety symptoms
  
  # Conners ADHD measures
  "zmhconnt1",    # Conners ADHD (inattention only)
  
  # ==========================================
  # Auxiliary Variables
  # ==========================================
  
  # Demographic variables
  # "sex1",         # Sex of twin 1
  # "zygos",        # Zygosity
  "sexzyg",       # Sex and zygosity combined
  # "x3zygos",      # 3-category zygosity
  "cohort",       # Birth cohort
  
  # Genotyping variables
  "genotyped1"   # Genotyped data available for twin 1
  # "DZtwinpair",   # DZ twin pair 
  # "genotypedzyg"  # Genotyped zygosity
)

# Missing Data
# rq5z[!(rq5z %in% colnames(df))]

