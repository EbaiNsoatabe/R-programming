blackjack <- function(card1, card2){
    if(card1 == 0 | card2 == 0){
      print("No zero values in blackjack")
    }else if(card1 < 22 & card2 < 22){
        if(card1 > card2){
          print(card1)
        } else if(card2 > card1){
            print(card2)
        } else{
          print("Draw")
        }
    }else{
      if(card1 > 21 & card2 <= 21){
        print(card2)
      } else if(card2 > 21 & card1 <= 21){
        print(card1)
      }else{
      print(0)
      }
    }
}