library(DiagrammeR)

DiagrammeR("graph LR
           C1(Development cost) --> C((Cost))
           C2(Maintenance cost) --> C
           C3(Training cost) --> C
           C4(Production cost) --> C
           C5(Free game for student) --> C
           B1(Incomes) --> B((Benefit))
           B2(Savings) --> B
           B3(Extracurricular activities) --> B2
           B4(Formal nutrition course) --> B2
           B5(Sales) --> B1
           B6(Good reputation) --> B1
           B7(Health benefit) --> B
           B8(Improved nutrition knowledge) --> B7
           B9(Positive behavioural change) --> B7
           R1(Parents don't like) --> B5
           R2(Students don't like) --> B6
           R2 --> B9
           R3(Bad learning outcome) --> B8
           varC((Cost))
           varB((Benefit))
           varR(Risk)
           C --> N{NPV}
           B --> N
           
           style C fill: lightyellow, stroke: yellow, stroke-width: 2px;
           style C1 fill: lightyellow, stroke: yellow, stroke-width: 2px;
           style C2 fill: lightyellow, stroke: yellow, stroke-width: 2px;
           style C3 fill: lightyellow, stroke: yellow, stroke-width: 2px;
           style C4 fill: lightyellow, stroke: yellow, stroke-width: 2px;
           style C5 fill: lightyellow, stroke: yellow, stroke-width: 2px;
           style varC fill: lightyellow, stroke: yellow, stroke-width: 2px;
           style B fill: lightgreen, stroke: green, stroke-width: 2px;
           style B1 fill: lightgreen, stroke: green, stroke-width: 2px;
           style B2 fill: lightgreen, stroke: green, stroke-width: 2px;
           style B3 fill: lightgreen, stroke: green, stroke-width: 2px;
           style B4 fill: lightgreen, stroke: green, stroke-width: 2px;
           style B5 fill: lightgreen, stroke: green, stroke-width: 2px;
           style B6 fill: lightgreen, stroke: green, stroke-width: 2px;
           style B7 fill: lightgreen, stroke: green, stroke-width: 2px;
           style B8 fill: lightgreen, stroke: green, stroke-width: 2px;
           style B9 fill: lightgreen, stroke: green, stroke-width: 2px;
           style varB fill: lightgreen, stroke: green, stroke-width: 2px;
           style N fill: lightblue, stroke: blue, stroke-width: 2px;
           style varR fill: pink, stroke: red, stroke-width: 2px;
           style R1 fill: pink, stroke: red, stroke-width: 2px;
           style R2 fill: pink, stroke: red, stroke-width: 2px;
           style R3 fill: pink, stroke: red, stroke-width: 2px
           
           ")
           