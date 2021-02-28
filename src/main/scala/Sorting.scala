import scala.annotation.tailrec

object Sorting {
// method for sort an array without using any mutable variable:
  def sort(arr:Array[Int]):Array[Int]={
    // Nested method to find the maximum element among the list of Integers:
    def max_Pos(list:List[Int]):Int= {
      //This method take 3 parameters maximum value(max_value),position(pos),maximum value's position(max_pos):
      /* In this method it take initial max value as the head of the list and the max value's position as 1.
         It will compare the the max value which was set initially through parameter with every other element
          of the list using recursion.
         It will take the position value as the length of the list because the maximum element = list.length
         Every time it compare with the any element of list it will decrease the position value by one
      */
      @tailrec
      def find_max_pos(max_value: Int, pos: Int, max_pos:Int): Int = {
        if (pos == 0) max_pos // if pos == 0 means every element is compared with each other
        else {
          if (max_value < list(pos - 1)) {
            find_max_pos(list(pos - 1), pos - 1,pos) // if initialized value is less then any other value it will call itself again with updated values
          }
          else{
            find_max_pos(max_value, pos - 1,max_pos) // else only the position value will be update
          }
        }
      }
      find_max_pos(list.head,list.length,1)
    }
    /* Method for sorting algorithm.
       In this it takes two list as the parameters First one is the resultant and the second will be the given list
       this method recursively call itself with updated parameters to get the sorted array
       In this I extract the largest value from the given array/list and add it in the new list
    */
    @tailrec
    def sorting(list_new: List[Int], list_old:List[Int]):Array[Int]={
      if(list_old.isEmpty) list_new.toArray
      else{
        val max_value = max_Pos(list_old) //find maximum value's position
        // resultant result updation is same in every condition
        if(max_value == 1) // Compare the Position and update the Given array for recursive call
          sorting(list_old(max_value - 1) :: list_new,list_old.drop(1)) // list_old.drop(1) means return all values except 1 from beginning
        else if(max_value == list_old.length)
          sorting(list_old(max_value - 1) :: list_new,list_old.dropRight(1))// list_old.dropRight(1) means return all values except 1 from last
        else {
          sorting(list_old(max_value-1) :: list_new,list_old.dropRight((list_old.length-max_value)+1):::list_old.drop(max_value))
          //(list_old.length-max_value)+1):::list_old.drop(max_value) means values that are on left side of Maximum value + means values that are on right side of Maximum value
        }
      }
    }
    sorting(Nil,arr.toList)
  }

  def main(args: Array[String]): Unit = {
    val array:Array[Int] = Array(6,5,4,3,2,1)
    println(array.toList)
    println(sort(array).toList)
    println()
    val array1:Array[Int] = Array(1,5,8,3,2,7)
    println(array1.toList)
    println(sort(array1).toList)
    println()
    val array2:Array[Int] = Array(6,5,8,3,4,1)
    println(array2.toList)
    println(sort(array2).toList)
    println()
    val array3:Array[Int] = Array(10,6,9,5,4,3,2,1,9)
    println(array3.toList)
    println(sort(array3).toList)
    println()
  }
}