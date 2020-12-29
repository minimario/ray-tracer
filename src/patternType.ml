type pattern_type = 
| StripePattern of Color.color * Color.color
| TestPattern
| GradientPattern of Color.color * Color.color
| RingPattern of Color.color * Color.color
| CheckersPattern of Color.color * Color.color

type pattern = {pattern_type: pattern_type; 
              transform: Matrix.matrix;}

exception BadPattern of string
