## Este archivo contiene los llamados a la API para descargar los datsets a utilizar:

informacion <- stack_info()
save(informacion,file = "informacion.Rdata")

uestions <- stack_questions(pagesize=100,num_pages=100,fromdate=1420070400,todate=1451606400,filter = "!9YdnSIN18")
save(questions,file = "questions_2015.Rdata")

tags <- stack_tags(pagesize = 100, num_pages = 5)   # Estos son los que faltan descargar
questions_2000 <- stack_search("probability",pagesize=100,num_pages=20,filter = "!9YdnSIN18")
save(questions_2000, file = "questions 2000.Rdata")
save(tags, file = "tags.Rdata")


closed.questions <- stack_search("probability", closed = T, pagesize=100,num_pages=10,fromdate=1420070400,todate=1451606400,filter = "!9YdnSIN18")
noclosed.questions <-  stack_search("probability", closed = F, pagesize=100,num_pages=10,fromdate=1420070400,todate=1451606400,filter = "!9YdnSIN18")
save(closed.questions, file = "closed questions.Rdata")
save(noclosed.questions, file = "noclosed questions.Rdata")


proba <- stack_search("",pagesize=100,num_pages=80,fromdate=1420070400,todate=1451606400,tagged = "probability")
analisis <- stack_search("",pagesize=100,num_pages=20,fromdate=1420070400,todate=1451606400, tagged = "real-analysis")
algebra <- stack_search("",pagesize=100,num_pages=20,fromdate=1420070400,todate=1451606400, tagged = "linear-algebra")
calculus <- stack_search("",pagesize=100,num_pages=20,fromdate=1420070400,todate=1451606400, tagged = "calculus")
equations <- stack_search("",pagesize=100,num_pages=20,fromdate=1420070400,todate=1451606400, tagged = "differential-equations")

save(proba, file = "proba.Rdata")
save(analisis, file = "analisis.Rdata")
save(algebra, file = "algebra.Rdata")
save(calculus, file = "calculus.Rdata")
save(equations, file = "equations.Rdata")
