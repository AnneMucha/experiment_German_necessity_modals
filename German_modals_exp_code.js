PennController.ResetPrefix(null)

Header(
   // void
)
.log( "PROLIFIC_ID" , GetURLParameter("id") )

DebugOff() //only for publishing

Sequence("introduction", "data","consent", "commitment", "instructions-SPR", "practice-SPR", "transition-SPR", rshuffle(randomize("SPR-trial-plain"), randomize("SPR-trial-attention")), "break", "instructions-accept", "practice-accept", "transition-accept", rshuffle(randomize("accept-trial-list1"), randomize("accept-trial-list2")), "Background-language", "Background-demographic", "send", "prolific")

// Instructions
newTrial("introduction",
    defaultText
        .print()
    ,
    newText("header-1", "<h2> Willkommen zu unserer Sprachstudie!</h2>")
    ,
    newText("header-2", "<h3>Zweck der Befragung:</h3>")
    ,
    newText("text-2", "In dieser Studie untersuchen wir, wie Sprecherinnen und Sprecher des Deutschen Sätze verarbeiten und bewerten. Die Studie ist Teil unserer Forschung zu Ähnlichkeiten und Unterschieden zwischen deutschen Muttersprachlern und Deutschlernenden.")
    ,
    newText("header-3", "<h3>Teilnahmevoraussetzung:</h3>")
    ,
    newText("text-3", "Um an dieser Studie teilnehmen zu können, müssen Sie Deutsch als Muttersprache sprechen und aktuell in Deutschland leben.")
    ,
    newText("header-4", "<h3>Ablauf:</h3>")
    ,
    newText("text-4", "<b>Die Studie umfasst 2 Teilexperimente und wird insgesamt etwa 35-40 Minuten in Anspruch nehmen. Bevor Sie mit der Bearbeitung anfangen, möchten wir Sie über den Ablauf informieren.</b>")
    ,
    newText("text-5","<li> Als Erstes klären wir Sie über die Verarbeitung Ihrer Daten auf.</li>")
    ,
    newText("text-6","<li> Dann geben wir Ihnen eine kurze Anleitung zur Bearbeitung der ersten Aufgabe.</li>")
    ,
    newText("text-7", "<li> Sie durchlaufen zwei Übungsdurchgänge, um sich mit der Aufgabe vertraut zu machen.</li>")
    ,
    newText("text-8", "<li> Nach der Übung beginnt das erste Teilexperiment.</li>")
    ,
    newText("text-pause", "<li>Falls notwendig, können Sie zwischen den beiden Teilexperimenten eine kurze Pause einlegen.</li>")
    ,
    newText("text-9", "<li>Dann geben wir Ihnen einige Instruktionen für die Bearbeitung der zweiten Aufgabe.</li>")
    ,
    newText("text-10", "<li> Dann beginnt das zweite Teilexperiment. </li>")
    ,
    newText("text-11", "<li> Zum Schluss erfassen wir ein paar Informationen zu Ihrem Hintergrund (z.B. Alter, Sprachkenntnisse). </li>")
    ,
    newButton("wait-intro", "Weiter")
        .center()
        .print()
        ,
        
    newCanvas("empty canvas", 1, 40) //add some space below the Continue button
    .print()
    ,
    getButton("wait-intro")
    .wait()
)

// Data handling
newTrial("data",
    defaultText
        .print()
    ,
    newText("h1", "<h2>Datenschutzerklärung</h2>")
    ,
    newText("data-1", "<b>Vertraulichkeit und Verwendung der Daten.</b> Alle im Laufe der Studie gesammelten Informationen werden gemäß dem Datenschutzgesetz verarbeitet. Um Ihre Privatsphäre zu schützen, geben wir niemals persönliche Informationen (wie Ihren Namen) an Personen außerhalb des Forschungsteams weiter. Ihre Daten werden mit einer eindeutigen Identifikationsnummer und nicht mit Ihrem Namen gekennzeichnet. Bitte beachten Sie, dass wir Ihre Identifikationsnummer vorübergehend erfassen, um eine wiederholte Teilnahme zu verhindern. Wir geben diese Informationen jedoch niemals an Personen außerhalb des Forschungsteams weiter. Wir speichern alle personenbezogenen Daten mithilfe des verschlüsselten Speicherdienstes der University of Edinburgh. Die im Rahmen dieser Studie gesammelten anonymisierten Daten werden für Forschungszwecke verwendet und in einem öffentlich zugänglichen Datenspeicher gespeichert.")
    ,
    newText("data-2", "<p> </p> <b>Welche Datenschutzrechte habe ich?</b> Die University of Edinburgh ist verantwortlich für die von Ihnen bereitgestellten Informationen. Sie haben das Recht, auf die über Sie gespeicherten Informationen zuzugreifen. Ihr Zugriffsrecht können Sie gemäß dem Datenschutzgesetz ausüben. Sie haben auch andere Rechte, darunter das Recht auf Berichtigung, Löschung und Widerspruch. Weitere Informationen, einschließlich des Rechts, eine Beschwerde beim <i>Information Commissioner's Office</i> einzureichen, finden Sie unter <a href= 'https://ico.org.uk/'>www.ico.org.uk</a>. Fragen, Kommentare und Anfragen zu Ihren persönlichen Daten können auch an den Datenschutzbeauftragten der Universität unter <a href='mailto:dpo@ed.ac.uk'>dpo@ed.ac.uk</a> gesendet werden.")
    ,
    newText("data-3", "<p> </p> <b>Freiwillige Teilnahme und Rücktrittsrecht.</b> Ihre Teilnahme ist freiwillig und Sie können aus beliebigen Gründen jederzeit während Ihrer Teilnahme oder innerhalb eines Monats nach Abschluss der Studie von der Studie zurücktreten. Wenn Sie während oder nach der Datenerfassung von der Studie zurücktreten, löschen wir Ihre Daten und es entstehen Ihnen keine Nachteile oder der Verlust von Vorteilen, auf die Sie ansonsten Anspruch haben.")
    ,
    newButton("wait-data", "Weiter zur Einverständniserklärung")
        .center()
        .print()
        ,
    newCanvas("empty canvas-2", 1, 40) //add some space below the Continue button
    .print()
    ,
    getButton("wait-data")
    .wait()
)
    newTrial("consent",
        defaultText
        .print()
    ,
    newText("data-5", "<p> </p> Indem Sie mit diesem Experiment fortfahren, stimmen Sie Folgendem zu:")
    ,
    newText("data-6", "<p> </p> <li> <b>Ich bin damit einverstanden, an dieser Studie teilzunehmen. </b> </li>")
    ,
    newText("data-7", "<li>Ich habe gelesen und verstanden, <b>wie meine Daten gespeichert und genutzt werden.</b> </li>")
    ,
    newText("data-8", "<li>Mir ist bewusst, <b>dass ich das Recht habe, diese Sitzung jederzeit abzubrechen. Wenn ich nach Abschluss der Studie zurücktrete</b>, werden meine Daten zu diesem Zeitpunkt gelöscht. </li> <p> </p>")
    ,
    newButton("wait-consent", "Einwilligen und weiter")
        .center()
        .print()
        .wait()
)



newTrial("commitment",
    newText("commit-1", "<h2> Selbstverpflichtung </h2>")
    .print()
    ,
    newText("commit-2", "Die Qualität unserer Daten ist uns wichtig. Damit wir korrekte Schlüsse aus den Studienergebnissen ziehen können, sind wir darauf angewiesen, dass Sie die Aufgaben in diesem Experiment aufmerksam und gewissenhaft erledigen.")
    .print()
    ,
    newText("<p>Versichern Sie, die Aufgaben in diesem Experiment aufmerksam und gewissenhaft zu erledigen?</p>")
    .bold()
    .print()
    ,
    newScale("response", " Ja ", " Nein ")
        .labelsPosition("right")
        .radio()    
        .center()
        .vertical()
        .once()
        .print()
        .log()      
    ,
    newCanvas("empty canvas-commit", 1, 40) 
    .print()
    ,
    // Initially hidden 'Continue' button
    newButton("continue", "Vielen Dank! Weiter zur Anleitung")
       .center()
    ,
    // If 'No' is selected, show a message and abort the experiment
    newText("abort-message", "Sie können leider nicht an dem Experiment teilnehmen. Bitte schließen Sie die Seite.")
        .color("red")
        .bold()
        .center()
        ,
    getScale("response").wait() 
            .test.selected(" Ja ")
            .success( getButton("continue").print())
            .failure( getText("abort-message").print() ) 
    ,
    // Wait for the "Continue" button to be clicked (if it becomes visible)
    getButton("continue").wait()
)


// instructions for the SPR part
newTrial("instructions-SPR",
    defaultText
        .print()
    ,
    newText("h-spr", "<h2>Anleitung zum ersten Teil der Studie</h2>")
    ,
    newText("t-spr-1", "Beim ersten Teil der Studie handelt es sich um ein so genanntes <i>self-paced reading</i>-Experiment.")
    ,
    newText("t-spr-2", "Dabei präsentieren wir Ihnen zunächst einen kurzen <b>Kontext</b>, der eine Situation beschreibt.  Lesen Sie den Kontext bitte sorgfältig und drücken Sie dann die Leertaste, um zum <b>Testsatz</b> zu gelangen.")
    ,
    newText("t-spr-4", "Hier sehen Sie zunächst nur einen Strich. Jedes Mal, wenn Sie die <b>Leertaste</b> drücken, wird ein neuer Teil des Satzes angezeigt.")
    ,
    newText("t-spr-6", "<p>Jeder Testsatz beginnt mit einer Redeeinleitung (z.B. <i>Hans sagt:</i>). Ihre Aufgabe ist, den gesamten Satz zu lesen, indem Sie die Leertaste drücken, um den jeweils folgenden Abschnitt aufzudecken. Versuchen Sie bitte, den Satz in ihrem normalen Lesetempo zu lesen. Sie können sich so viel Zeit nehmen, wie Sie für das Lesen jedes Abschnitts benötigen, sollten aber unnötig lange Pausen vermeiden.</p>")
    ,
    newText("t-spr-7", "Nicht alle Sätze werden Ihnen in dem angegebenen Kontext gleichermaßen sinnvoll erscheinen. Das ist so beabsichtigt, denn uns interessiert unter anderem, wie sich bestimmte Eigenschaften der Sätze auf das Leseverhalten auswirken.")
    ,
    newText("t-spr-9", "<p>Es ist wichtig, dass Sie sowohl den Kontext als auch den Testsatz sorgfältig lesen. Daher stellen wir Ihnen nach einigen Testsätzen eine Frage, um Ihr Verständnis zu überprüfen. Bitte beantworten Sie die Frage, indem Sie eine der drei angegebenen Optionen auswählen. Sie können die richtige Antwort entweder anklicken oder auf Ihrer Tastatur die Taste für die entsprechende Nummer ('1', '2' oder '3') drücken.</p>") 
    ,
    newText("t-spr-10", "Im nächsten Schritt präsentieren wir Ihnen zwei Übungsaufgaben, damit Sie sich mit der Aufgabe vertraut machen können.")
    ,
    newButton("wait-instr", "zu den Übungsaufgaben")
        .center()
        .print()
    ,
    newCanvas("empty canvas-3", 1, 40) //add some space below the Continue button
    .print()
    ,
    getButton("wait-instr")
    .wait()
)

//Practice trials
Template("practice.csv", row =>
    newTrial("practice-SPR",
    newText("item-p", "<b>Kontext:</b> " + row.context + "<p> </p>" + "<i>(weiter mit Leertaste) </i>")
        .center()
        .print()
    ,
    newKey("keypress", " ") 
        .log()
        .wait()
        .center()
    ,
    getText("item-p")
    .remove()
    ,
    newTimer("wait-p", 300) 
    .start()
    .wait()
    ,
    newController("DashedSentence",  {s : row.sentence.split("~")
    , display: "in place"
    }) 
        .center()
        .print()
        .log()
        .wait(200)
        .remove()
        ,
    newController("Question",  {q : row.question
        , as: [row.option1,row.option2,row.option3] 
        , showNumbers: true 
        , randomOrder: true
        , hasCorrect: true // all correct answers as option1
    }) 
        .center()
        .print()
        .log()
        .wait()
    )
    .log("experiment", row.experiment)
    .log("item", row.item_name)
)


// transition from practice to experiment
newTrial("transition-SPR",
    defaultText
        //.center()
        .print()
    ,
    newText("transition-SPR-1", "Das waren die Übungsdurchgänge. Klicken Sie auf <b>Start</b>, um das eigentliche Experiment zu beginnen.")
    ,
    newButton("wait-SPR", "Start")
        .center()
        .print()
        .wait()
)


//SPR-trials without attention checks
Template("List-A_input_plain.csv", row =>
    newTrial("SPR-trial-plain",
    newText("item", "<b>Kontext:</b> " + row.context + "<p> </p>" + "<i>(weiter mit Leertaste) </i>")
        .center()
        .print()
    ,
    newKey("keypress", " ")
        .log()
        .wait()
        .center()
    ,
    getText("item")
    .remove()
    ,
    newTimer("wait", 300) 
    .start()
    .wait()
    ,
    newController("DashedSentence",  {s : row.sentence.split("~")
    , display: "in place"
    }) 
        .center()
        .print()
        .log()
        .wait(200)
        .remove()
    ,
    newText("enter", "Drücken Sie <b>Enter</b>, um zum nächsten Beispiel zu gelangen.")
    .print()
    .center()
    ,
    newKey("keypress-plain", "Enter") 
        .log()
        .wait()
        .center()
        ,
    getText("enter")
        .remove()
    ,
    )
    .log("group", row.group)
    .log("experiment", row.experiment)
    .log("item", row.item_name)
    .log("condition", row.condition)
    .log("condition_no", row.condition_no)
    .log("modal", row.modal)
    .log("parameter", row.parameter)
)

//SPR-trials with attention checks
Template("List-A_input_attention.csv", row =>
    newTrial("SPR-trial-attention",
    newText("item", "<b>Kontext:</b> " + row.context + "<p> </p>" + "<i>(weiter mit Leertaste) </i>")
        .center()
        .print()
    ,
    newKey("keypress", " ") 
        .log()
        .wait()
        .center()
    ,
    getText("item")
    .remove()
    ,
    newTimer("wait", 300) 
    .start()
    .wait()
    ,
    newController("DashedSentence",  {s : row.sentence.split("~")
    , display: "in place"
    }) 
        .center()
        .print()
        .log()
        .wait(200)
        .remove()
        ,
    newController("Question",  {q : row.question
        , as: [row.option1,row.option2,row.option3] 
        , showNumbers: true 
        , randomOrder: true
        , hasCorrect: true 
    }) 
        .center()
        .print()
        .log()
        .wait()
    )
    .log("group", row.group)
    .log("experiment", row.experiment)
    .log("item", row.item_name)
    .log("condition", row.condition)
    .log("condition_no", row.condition_no)
    .log("modal", row.modal)
    .log("parameter", row.parameter)
)
//optional break between self-paced reading and acceptability part
newTrial("break",
    newText("tired-1", "<h3>Das war der erste Teil der Studie.</h3>")
    .center()
    .print()
    ,
    newImage("cat", "cat_picture_small.jpg") // cute cat picture for fun
    .center()
    .print()
    ,
    newCanvas("empty canvas-2", 1, 40) 
    .print()
    ,
    newText("tired-2", "<b>Müde?</b> Dann können Sie jetzt eine kleine Pause machen. (Schließen Sie aber bitte <u>nicht</u> die Seite!) Wenn Sie bereit sind, das Experiment fortzusetzen, klicken Sie auf <b>weiter</b>, um zur Anleitung für den zweiten Teil zu gelangen.")
    .print()
    ,
    newButton("wait-break", "weiter")
    .center()
    .print()
    ,
    newCanvas("empty canvas-wait", 1, 40) 
    .print()
    ,
    getButton("wait-break")
    .wait()
)

// instructions for the acceptability part
newTrial("instructions-accept",
    defaultText
        .print()
    ,
    newText("accept-1", "<h2>Anleitung zum zweiten Teil der Studie</h2>")
    ,
    newText("accept-2", "Im <b>zweiten Teil der Studie</b> bitten wir sie, die Angemessenheit der gezeigten Äußerungen in den jeweiligen Kontexten zu bewerten. Dafür präsentieren wir Ihnen in diesem Teil die Kontexte und die Testsätze zusammen auf einer Seite.")
    ,
    newText("accept-3","<p>Ihre Aufgabe ist, die Kontexte und Testsätze nochmals aufmerksam durchzulesen und auf einer Skala von 1 bis 7 zu bewerten, <b>wie angemessen der Satz in dem jeweiligen Kontext ist.</b> Der Wert '1' auf der Skala bedeutet, der Satz ist in dem Kontext nicht angemessen. '7' bedeutet, der Satz ist in dem Kontext natürlich und voll angemessen. Die Zahlen 2-6 repräsentieren Nuancen zwischen 'nicht angemessen' und 'voll angemessen'.</p>")
    ,
    newText("accept-4", "Bei dieser Aufgabe gibt es keine richtigen oder falschen Antworten. Verlassen Sie sich einfach auf Ihre sprachliche Intuition darüber, ob der Satz in dem jeweiligen Kontext natürlich und angemessen klingt.")
    ,
    newText("accept-5", "<p>Sie können eine Zahl auf der Skala durch Anklicken auswählen oder die entsprechende Taste auf Ihrer Tastatur drücken.</p>")
    ,
    newText("accept-6", "Zunächst können Sie sich wieder in zwei Übungsdurchgängen mit der Aufgabe vertraut machen.")
    ,
    newCanvas("empty canvas-instr-acc", 1, 40) 
    .print()
    ,
    newButton("wait-instr2", "zu den Übungsaufgaben")
        .center()
        .print()
    ,
    newCanvas("empty canvas-instr-acc2", 1, 40) 
    .print()
    ,
    getButton("wait-instr2")
    .wait()
)
//practice acceptability
Template("practice.csv", row =>
    newTrial("practice-accept",
        newText("item", "<b>Kontext:</b> " + row.context + "<p> </p>")
            .center()
            .print()
        ,
        newController("AcceptabilityJudgment",  {s : row.sentence.replaceAll("~"," ")
        , presentAsScale: true
        , as: [["1", "1"], ["2", "2"], ["3", "3"], ["4", "4"], ["5", "5"], ["6", "6"], ["7", "7"]]
        , showNumbers: true 
        , randomOrder: false
        , leftComment: "nicht<br> angemessen"
        , rightComment: "voll<br> angemessen"
    }) 
        .center()
        .print()
        .log()
        .wait()
    )
    .log("experiment", row.experiment)
    .log("item", row.item_name)
)

// transition from practice to experiment
newTrial("transition-accept",
    defaultText
        .center()
        .print()
    ,
    newText("transition-accept-1", "Das waren die Übungsdurchgänge. Klicken Sie auf <b>weiter</b>, um fortzufahren.")
    ,
    newCanvas("empty canvas", 1, 40)
    .print()
    ,
    newButton("wait-accept", "weiter")
        .center()
        .print()
        .wait()
)

//acceptability judgments for non-attention list
Template("List-A_input_plain.csv", row =>
    newTrial("accept-trial-list1",
        newText("item", "<b>Kontext:</b> " + row.context + "<p> </p>")
            .center()
            .print()
        ,
        newController("AcceptabilityJudgment",  {s : row.sentence.replaceAll("~"," ")
        , presentAsScale: true
        , as: [["1", "1"], ["2", "2"], ["3", "3"], ["4", "4"], ["5", "5"], ["6", "6"], ["7", "7"]]
        , showNumbers: true 
        , randomOrder: false
        , leftComment: "nicht<br> angemessen"
        , rightComment: "voll<br> angemessen"
    }) 
        .print()
        .log()
        .wait()
    )
    .log("group", row.group)
    .log("experiment", row.experiment)
    .log("item", row.item_name)
    .log("condition", row.condition)
    .log("condition_no", row.condition_no)
    .log("modal", row.modal)
    .log("parameter", row.parameter)
)

//acceptability judgments for attention list
Template("List-A_input_attention.csv", row =>
    newTrial("accept-trial-list2",
        newText("item", "<b>Kontext:</b> " + row.context + "<p> </p>")
            .center()
            .print()
        ,
        newController("AcceptabilityJudgment",  {s : row.sentence.replaceAll("~"," ")
        , presentAsScale: true
        , as: [["1", "1"], ["2", "2"], ["3", "3"], ["4", "4"], ["5", "5"], ["6", "6"], ["7", "7"]]
        , showNumbers: true 
        , randomOrder: false
        , leftComment: "nicht<br> angemessen"
        , rightComment: "voll<br> angemessen"
    }) 
        .print()
        .log()
        .wait()
    )
    .log("group", row.group)
    .log("experiment", row.experiment)
    .log("item", row.item_name)
    .log("condition", row.condition)
    .log("condition_no", row.condition_no)
    .log("modal", row.modal)
    .log("parameter", row.parameter)
  //  .log("ID", getVar("ID"))
)

// Questions on language and demographic background
newTrial("Background-language",
    newText("text-BG", "<b>Vielen Dank für die Bearbeitung!</b> Zum Schluss möchten wir noch einige Informationen zu Ihrem Hintergrund erfassen. Diese Informationen sind hilfreich für uns, um eventuelle Unterschiede zwischen Sprecher*innen besser zu verstehen.")
    .print()
    ,
    newText("h-languages", "<h3>Sprachenprofil </h3>")
    .print()
    ,
    newText("<p>Sprechen Sie, nach eigener Einschätzung, einen bestimmten Dialekt des Deutschen?</p>")
    .print()
    ,
    newDropDown("dialect-choice", " ")
            .add("ja", "nein")
            .print()
            .log()
    ,
    newText("<p>Falls 'ja', welchen Dialekt sprechen Sie?</p>")
    .print()
    ,
    newTextInput("dialect-free", " ")
    .log()
    .lines(0)
    .size(400, 30)
    .print()
    ,
    newText("<p>Haben Sie, abgesehen von Deutsch, noch weitere Sprachen gelernt?</p>")
    .print()
    ,
    newDropDown("languages-choice", " ")
            .add("ja", "nein")
            .print()
            .log()
    ,
    newText("<p>Falls 'ja', welche Sprache(n) haben Sie gelernt und wie lange? (z.B.: <i>Englisch (5 Jahre)</i>)</p>")
    .print()
    ,
    newTextInput("languages-free", " ")
    .lines(0)
    .size(400, 50)
    .print()
    .log()
    ,
    newText("<p>Haben Sie schonmal länger als 6 Monate im nicht-deutschsprachigen Ausland gelebt? </p>")
    .print()
    ,
    newDropDown("abroad-choice", " ")
            .add("ja", "nein")
            .print()
            .log()
    ,
    newText("<p>Falls 'ja', wo haben Sie gelebt und wie lange? (z.B.: <i>USA (1 Jahr)</i>)</p>")
    .print()
    ,
    newTextInput("abroad-free", " ")
    .lines(0)
    .size(400, 50)
    .print()
    .log()
    ,
    newCanvas("empty canvas-language", 1, 40) 
    .print()
    ,
newButton("send-BG-language", "Weiter")
    .print()
    , 
    newCanvas("empty canvas-language2", 1, 40) 
    .print()
    ,
    getButton("send-BG-language")
    .wait()
    ,
    newVar("abroad-free-text")
        .set(getTextInput("abroad-free"))
    ,
    newVar("languages-free-text")
        .set(getTextInput("languages-free"))
).setOption("countsForProgressBar",false)

newTrial("Background-demographic",
    newText("h-demographic", "<h3>Demografische Daten </h3>")
    .print()
    ,
    newText("text-gender", "<p>Welchem Geschlecht ordnen Sie sich zu?</p>")
    .print()
    ,
    newDropDown("sex", "Geschlecht")
            .add("weiblich", "männlich", "divers")
            .print()
            .log()
    ,
    newText("text-age", "<p>Wie alt Sind Sie?</p>")
    .print()
    ,
    newDropDown("age", "Alter")
            .add("18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", "43", "44", "45", "46", "47", "48", "49", "50", "51", "52", "53", "54", "55", "56", "57", "58", "59", "60", "61", "62", "63", "64", "65", "66", "67", "68", "69", "70", "71", "72", "73", "74", "75", "76", "77", "78", "79", "80", "älter als 80")
            .print()
            .log()
    ,
    newText("h-commentary", "<h3>Weitere Kommentare</h3>")
    .print()
    ,
    newText("t-feedback", "<p>In dieses Kästchen können Sie (optional) noch zusätzliche Kommentare schreiben. Klicken Sie 'Weiter', um zum Ende des Experiments zu gelangen.</p>")
    .print()
    ,
    newTextInput("feedback", " ")
    .lines(0)
    .size(400, 100)
    .print()
    .log()
,
    newCanvas("empty canvas-demo", 1, 40) 
    .print()
    ,
    newButton("send-BG-demo", "Weiter")
    .print()
    ,
    newCanvas("empty canvas-demo2", 1, 40) 
    .print()
    ,
    getButton("send-BG-demo")
    .wait()
    ,
    newVar("feedback-text")
        .set(getTextInput("feedback"))
)

// Send results to server
SendResults("send")

//redirection to prolific
 newTrial("prolific",
    newText("<p><b>Vielen Dank für Ihre Teilnahme!</b></p>")
        .center()
        .print()
    ,
    newText("<p><a href='https://app.prolific.com/submissions/complete?cc=C10P48GS'"+ GetURLParameter("id")+"' target='_blank'>Klicken Sie hier, um Ihre Teilnahme auf Prolific zu bestätigen.</a></p> <p>Dieser Schritt ist notwendig, um Ihre Vergütung zu bekommen!</p>")
        .center()
        .print()
    ,
    newButton("void")
        .wait()
    )

        
