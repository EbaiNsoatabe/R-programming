unique <- function(num1, num2, num3){
  if(num1 != num2 & num2 != num3 & num1 != num3){
    print(num1 + num2 + num3)
  } else if(num1 == num2 & num2 != num3){
    print(num3)
  } else if(num1 != num2 & num2 == num3){
    print(num1)
  } else if(num1 ==num3 & num2 != num3){
    print(num2)
  }else{
    print(0)
  }
}