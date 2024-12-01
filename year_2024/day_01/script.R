input <- read.table("input.txt")

#### Step 1 ####
(sort(input$V1) - sort(input$V2)) |> abs() |> sum()

#### Step 2 ####
(sapply(input$V1, \(x) sum(x == input$V2)) * input$V1) |> sum()
