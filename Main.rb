# frozen_string_literal: true

require 'prime'
require 'fileutils'
require 'time'
require 'date'
require 'matrix'
def task_7
  count = 0
  Prime.each(10_000_000_000) do |prime|
    count += 1
    if count == 10_001
      puts prime
      break
    end
  end
  puts count
end
def task_8

  number = '7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843 858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450'
  count = 13
  puts number
         .gsub(/\s+/, '') # remove whitespace
         .each_char # split into characters
         .map(&:to_i) # convert to integers
         .each_cons(count) # sliding window of subsequences
         .map { |subseq| subseq.reduce(:*) } # map each subsequence to its product
         .max
end
def task_9
  (1..1000).each do |a|
    (a + 1..1000).each do |b|
      c = 1000 - a - b
      p a * b * c if (a * a + b * b) == c * c
    end
  end
end
def task_10
  puts Prime.each(2_000_000).inject(&:+)
end
def task_11
  array = [
    [8, 2, 22, 97, 38, 15, 0, 40, 0, 75, 4, 5, 7, 78, 52, 12, 50, 77, 91, 8],
    [49, 49, 99, 40, 17, 81, 18, 57, 60, 87, 17, 40, 98, 43, 69, 48, 4, 56, 62, 0],
    [81, 49, 31, 73, 55, 79, 14, 29, 93, 71, 40, 67, 53, 88, 30, 3, 49, 13, 36, 65],
    [52, 70, 95, 23, 4, 60, 11, 42, 69, 24, 68, 56, 1, 32, 56, 71, 37, 2, 36, 91],
    [22, 31, 16, 71, 51, 67, 63, 89, 41, 92, 36, 54, 22, 40, 40, 28, 66, 33, 13, 80],
    [24, 47, 32, 60, 99, 3, 45, 2, 44, 75, 33, 53, 78, 36, 84, 20, 35, 17, 12, 50],
    [32, 98, 81, 28, 64, 23, 67, 10, 26, 38, 40, 67, 59, 54, 70, 66, 18, 38, 64, 70],
    [67, 26, 20, 68, 2, 62, 12, 20, 95, 63, 94, 39, 63, 8, 40, 91, 66, 49, 94, 21],
    [24, 55, 58, 5, 66, 73, 99, 26, 97, 17, 78, 78, 96, 83, 14, 88, 34, 89, 63, 72],
    [21, 36, 23, 9, 75, 0, 76, 44, 20, 45, 35, 14, 0, 61, 33, 97, 34, 31, 33, 95],
    [78, 17, 53, 28, 22, 75, 31, 67, 15, 94, 3, 80, 4, 62, 16, 14, 9, 53, 56, 92],
    [16, 39, 5, 42, 96, 35, 31, 47, 55, 58, 88, 24, 0, 17, 54, 24, 36, 29, 85, 57],
    [86, 56, 0, 48, 35, 71, 89, 7, 5, 44, 44, 37, 44, 60, 21, 58, 51, 54, 17, 58],
    [19, 80, 81, 68, 5, 94, 47, 69, 28, 73, 92, 13, 86, 52, 17, 77, 4, 89, 55, 40],
    [4, 52, 8, 83, 97, 35, 99, 16, 7, 97, 57, 32, 16, 26, 26, 79, 33, 27, 98, 66],
    [88, 36, 68, 87, 57, 62, 20, 72, 3, 46, 33, 67, 46, 55, 12, 32, 63, 93, 53, 69],
    [4, 42, 16, 73, 38, 25, 39, 11, 24, 94, 72, 18, 8, 46, 29, 32, 40, 62, 76, 36],
    [20, 69, 36, 41, 72, 30, 23, 88, 34, 62, 99, 69, 82, 67, 59, 85, 74, 4, 36, 16],
    [20, 73, 35, 29, 78, 31, 90, 1, 74, 31, 49, 71, 48, 86, 81, 16, 23, 57, 5, 54],
    [1, 70, 54, 71, 83, 51, 54, 69, 16, 92, 33, 48, 61, 43, 52, 1, 89, 19, 67, 48]
  ]

  padding = array.length - 1
  padded_matrix = []
  array.each do |row|
    inverse_padding = array.length - padding
    padded_matrix << ([nil] * inverse_padding) + row + ([nil] * padding)
    padding -= 1
  end

  res = 0

  padded_matrix.transpose.map(&:compact).each do |values|
    if values.length >= 4
      max = values.each_cons(4).map { |subseq| subseq.reduce(:*) }.max
      res = max if max > res
    end
  end

  result = []

  matrix = Matrix[*array]
  result << matrix.each_cons(4).map { |subseq| subseq.reduce(:*) }.max
  result << matrix.transpose.each_cons(4).map { |subseq| subseq.reduce(:*) }.max
  result << res

  p result.max
end
def task_12
  (1..1_000_000_000_000).each do |n|
    value = ((1..n).inject { |sum, n| sum + n })
    count = 0
    (1..Math.sqrt(value).round).each do |x|
      count += 2 if (value % x).zero?
    end
    if count >= 500
      puts value
      break
    end
  end
end
def get_list(file_name)
  data = []
  File.open(file_name, 'r') do |file|
    file.each_line { |x| data.push(x.to_i) }
  end
  data
end
def task_13
  puts (get_list('./task_13.txt').sum.to_s)[0..9]
end
def task_14
  max_count = 0
  first_value = 0

  (1..1_000_000).each do |n|
    start = n
    count = 1
    loop do
      break if start == 1

      start = if start.even?
                start / 2
              else
                3 * start + 1
              end
      count += 1
    end
    if count > max_count
      max_count = count
      first_value = n
    end
  end

  puts first_value
end
def task_15
  p ((21..40).inject(:*) / (1..20).inject(:*))
end
def task_16
  puts((2 ** 1000).to_s.each_char.map(&:to_i).reduce(:+))
end
def get_dic(file_name)
  dictionary = Hash.new('dictionary')
  File.open(file_name, 'r') do |file|
    file.each_line do |x|
      data = x.split(' ')
      dictionary[data[0]] = data[1]
    end
  end
  dictionary
end
def task_17
  total = 0
  dictionary = get_dic('./task_17.txt')
  (1..1000).each do |num|
    if num / 1000 >= 1
      total += dictionary['1000'].size + dictionary[(num / 1000).to_s].size
      next
    end
    if num / 100 >= 1
      total += dictionary['100'].size + dictionary[(num / 100).to_s].size
      if num % 100 >= 1
        total += 'and'.length
      else
        next
      end
    end
    num = num % 100

    if num > 20 && num % 10 != 0
      total += dictionary[((num / 10).to_i * 10).to_s].size
      num = num % 10
    end
    total += dictionary[num.to_s].length
  end
  puts total
end
def task_18
  input = [
    [75],
    [95, 64],
    [17, 47, 82],
    [18, 35, 87, 10],
    [20, 4, 82, 47, 65],
    [19, 1, 23, 75, 3, 34],
    [88, 2, 77, 73, 7, 63, 67],
    [99, 65, 4, 28, 6, 16, 70, 92],
    [41, 41, 26, 56, 83, 40, 80, 70, 33],
    [41, 48, 72, 33, 47, 32, 37, 16, 94, 29],
    [53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14],
    [70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57],
    [91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48],
    [63, 66, 4, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31],
    [4, 62, 98, 27, 23, 9, 70, 98, 73, 93, 38, 53, 60, 4, 23]
  ]

  n = input.length
  i = n - 2
  loop do
    (0..i).each do |j|
      input[i][j] += if input[i + 1][j] > input[i + 1][j + 1]
                       input[i + 1][j]
                     else
                       input[i + 1][j + 1]
                     end
    end
    break if i.negative?

    i -= 1
  end

  puts input[0][0]
end
def task_19
  count = 0
  date = Date.new(1901, 1, 1)
  while date.year != 2001
    count += 1 if date.wday.zero?
    date = date.next_month
  end
  puts count
end
def task_20
  puts((1..100).reduce(:*).to_s.each_char.map(&:to_i).reduce(:+))
end
def names_scores(name, index)
  alphabet = {
    'A' => 1,
    'B' => 2,
    'C' => 3,
    'D' => 4,
    'E' => 5,
    'F' => 6,
    'G' => 7,
    'H' => 8,
    'I' => 9,
    'J' => 10,
    'K' => 11,
    'L' => 12,
    'M' => 13,
    'N' => 14,
    'O' => 15,
    'P' => 16,
    'Q' => 17,
    'R' => 18,
    'S' => 19,
    'T' => 20,
    'U' => 21,
    'V' => 22,
    'W' => 23,
    'X' => 24,
    'Y' => 25,
    'Z' => 26
  }
  result = 0
  name.each_char.map do |letter|
    result += alphabet[letter]
  end
  result * index
end
def task_21
  sum = []
  (1..10_000).each do |i|
    divisors = (1..i - 1).select { |n| (i % n).zero? }
    sum_of_divisors = divisors.sum
    next unless sum_of_divisors != i

    new_divisors = (1..sum_of_divisors - 1).select { |n| (sum_of_divisors % n).zero? }
    sum_of_new_divisors = new_divisors.sum
    sum << sum_of_divisors << sum_of_new_divisors if sum_of_new_divisors == i
  end
  puts sum.uniq.sum
end
def task_22
  sum = 0
  values = File.new('./names.txt', 'r:UTF-8').readlines.sample.to_s.split(',').sort
  values.each do |value|
    sum += names_scores(value, (values.index(value) + 1))
  end
  p sum
end
def proper_factors(n)
  first, *rest = Prime.prime_division(n).map { |m, pow| (0..pow).map { |p| m ** p } }
  first.product(*rest).map { |arr| arr.reduce(:*) } - [n]
end
def task_23
  max_val = 28_123
  abundant = (12..max_val).select { |n| proper_factors(n).sum > n }

  abundant.size

  half_max = max_val / 2
  last_abundant_idx = abundant.size - 1

  sum_nbrs_sum_two = abundant.each_with_index.with_object([]) do |(n, i), found|
    break found if n > half_max

    (i..last_abundant_idx).each do |j|
      m = n + abundant[j]
      break if m > max_val

      found << m
    end
  end.uniq.sum

  all_sum = max_val * (1 + max_val) / 2
  p all_sum - sum_nbrs_sum_two
end
def task_24
  numbers = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
  p numbers.permutation(numbers.length).to_a[999_999]
end
def task_25
  index = 2
  cur = 1
  prev = 1
  loop do
    index += 1
    b = cur
    cur += prev
    prev = b
    break if cur.to_s.size == 1000
  end
  puts index
end                               
def task_26
  sq_length = 0
  i = 10
  loop do
    found_rm = (0..i).to_a.map { 0 }
    value = 1
    pos = 0
    while (found_rm[value]).zero? && !value.zero?
      found_rm[value] = pos
      value *= 10
      value %= i
      pos += 1
    end
    sq_length = pos - found_rm[value] if pos - found_rm[value] > sq_length
    break if sq_length >= i || i == 1
    i -= 1
  end
  p i + 1
end
def numberOfConsecutivePrimesGenerated(a, b)
  (0..Float::INFINITY).each do |i|
    n = i * i + i * a + b
    return i if n.negative? || !Prime.instance.prime?(n)
  end
end
def task_27
  best_num = 0
  best_A = 0
  best_B = 0
  (-1000..1000).each do |a|
    (-1000..1000).each do |b|
      num = numberOfConsecutivePrimesGenerated(a, b)
      if num > best_num
        best_num = num
        best_A = a
        best_B = b
      end
    end
  end
  p best_A * best_B
end
def task_28
  sum = 1
  (3..1002).step(2) do |n|
    sum += 4 * n * n - 6 * n + 6
  end
  p sum
end
def task_29
  n = 2
  sq_numbers = []
  while n < 101
    (2..100).each do |num|
      sq_numbers << (num ** n)
    end
    n += 1
  end
  sq_numbers = sq_numbers.uniq
  puts sq_numbers.size
end
def task_30
  result = 0
  sum = 0
  (2..1_000_000).each do |i|
    i.to_s.each_char.map(&:to_i).each do |val|
      sum += val ** 5
    end
    result += i if sum == i
    sum = 0
  end
  p result
end
def task_31
  t = 200
  r_ways = [1] + [0] * t
  coins = [1, 2, 5, 10, 20, 50, 100, 200]
  coins.each do |coin|
    (0..(r_ways.size - coin)).each do |i|
      r_ways[i + coin] += r_ways[i] if i + coin <r_ways.size
    end
  end
  puts r_ways[-1]
end
class Array
  def my_map
    new_arr = []
    if block_given?
      new_arr = self.each { |i| new_arr << yield(i) }
    else
      new_arr = to_enum :my_map
    end
    return new_arr
  end
  def my_map!
      self.each_with_index { |i, index| self[index] = yield(i) }
  end
  def my_reduce(res=self.shift, block)
    self.each do |i|
      res = res.send block, i
    end
    res
  end
end


def task_32
  puts (1..4999).flat_map { |a|
    (1..99).map do |b|
      [a.to_s + b.to_s + (a * b).to_s, a * b]
    end
  }.select { |p|
    p[0].length == 9 && p[0].each_char.sort.join == "123456789"
  }.map { |p| p[1] }.uniq.reduce(:+)
end

def task_35
  count=0
  prime_numbers = Prime.take_while { |p| p < 1_000_000 }
  prime_numbers.each do |prime|
    flag=true
    if(prime.to_s.size>1)
      i=0
      buf_str=prime.to_s
      while i!=prime.to_s.size
        buf_str=buf_str[1..buf_str.size] + buf_str[0]
        if !Prime.prime?(buf_str.to_i)
          flag = false
          break
        end
        i+=1
      end
    end
    if flag
      count+=1
      puts prime
    end
  end
  p count
end







def task_54
  suit = "SHCD"
  rank = "23456789TJQKA"
  res = -3

  File.open('./poker.txt', 'r') do |file|
    file.each_line do |x|
      card_1 = []
      card_2 = []
      data = x.split(' ')
      card_1 = data[0..4]
      card_2 = data[5..9]
      res += 1 if player1_win(card_1, card_2)
    end
  end
  puts res
end
def player1_win(hand1, hand2)
  rank_hash = { "2" => 2, "3" => 3, "4"=>4, "5"=>5, "6"=>6, "7"=>7, "8"=>8, "9"=>9, "T"=>10, "J"=>11, "Q"=>12, "K"=>13, "A"=>14 }
  hand1 = hand1.sort_by!{|e| rank_hash[e[0]]}
  hand2 = hand2.sort_by!{|e| rank_hash[e[0]]}
  hand1_score = combo_score(hand1)
  hand2_score = combo_score(hand2)


  if hand1_score > hand2_score
    return true
  elsif hand1_score == hand2_score
    return solution_to_controversial(hand1, hand2, hand1_score, rank_hash)
  end
  return false
end
def solution_to_controversial(hand1,hand2,combo_score,rank_hash)
  case combo_score
  when 9
    return rank_hash[hand1[-1][0]] > rank_hash[hand2[-1][0]]
  when 8
    return rank_hash[is_four_of_kind(hand1)] > rank_hash[is_four_of_kind(hand2)]
  when 7
    return rank_hash[is_set(hand1)] > rank_hash[is_set(hand2)]
  when 6
    i = -1
    high_1 = hand1[i][0]
    high_2 = hand2[i][0]
    while high_1 == high_2
      i-=1
      high_1 = hand1[i][0]
      high_2 = hand2[i][0]
    end
    return rank_hash[high_1] > rank_hash[high_2]
  when 5
    return rank_hash[hand1[-1][0]] > rank_hash[hand2[-1][0]]
  when 4
    return rank_hash[is_set(hand1)] > rank_hash[is_set(hand2)]
  when 3
    if rank_hash[pair(hand1)] > rank_hash[pair(hand2)]
      return true
    elsif  rank_hash[pair(hand1)] == rank_hash[pair(hand2)]
      hand1.delete(pair(hand1))
      hand2.delete(pair(hand2))
      if(rank_hash[pair(hand1)] > rank_hash[pair(hand2)])
        return true
      elsif rank_hash[pair(hand1)] == rank_hash[pair(hand2)]
        hand1.delete(pair(hand1))
        hand2.delete(pair(hand2))
        return true if rank_hash[hand1[0]] > rank_hash[hand2[0]]
      end
    end
    return false
  when 2
    if rank_hash[pair(hand1).chr] > rank_hash[pair(hand2).chr]
      return true
    elsif  rank_hash[pair(hand1)] == rank_hash[pair(hand2)]
      hand1.delete(pair(hand1))
      hand2.delete(pair(hand2))
      i = -1
      high_1 = hand1[i][0]
      high_2 = hand2[i][0]
      while high_1 == high_2
        i-=1
        high_1 = hand1[i][0]
        high_2 = hand2[i][0]
      end
      return true if rank_hash[high_1] > rank_hash[high_2]
    end
    return false
  when 1
    i = -1
    high_1 = hand1[i][0]
    high_2 = hand2[i][0]
    while high_1 == high_2
      i-=1
      high_1 = hand1[i][0]
      high_2 = hand2[i][0]
    end
    return rank_hash[high_1] > rank_hash[high_2]
  else
    p "Mistake"
  end
end
def combo_score(hand)
  rank_hash = { "2" => 2, "3" => 3, "4"=>4, "5"=>5, "6"=>6, "7"=>7, "8"=>8, "9"=>9, "T"=>10, "J"=>11, "Q"=>12, "K"=>13, "A"=>14 }
  if is_flush(hand)
    if is_straight(hand,rank_hash)
      if hand[hand.size-1][0] == "A"
        return  10
      end
      return 9
    end
    return 6
  end
  return 8 if is_four_of_kind(hand) != "0"
  return 7 if is_full_house(hand)
  return 5 if is_straight(hand, rank_hash)
  return 4 if is_set(hand) != "0"
  return 3 if is_two_pair(hand)
  return 2 if pair(hand) != "0"
  return 1
end
def pair(hand)
  value = "0"
  for i in 0..hand.size-2
    if(hand[i][0]==hand[i+1][0])
      value = hand[i][0]
    end
  end
  return value
end
def is_two_pair(hand)
  kinds = hand.map{|e| e[0]}.uniq
  return true if kinds.size == 3
  return false
end
def is_set(hand)
  kinds = hand.map{|e| e[0]}
  kinds_1 = kinds[0..kinds.size-3].uniq
  kinds_2 = kinds[1..kinds.size-2].uniq
  kinds_3 = kinds[2..kinds.size-1].uniq
  return kinds_1.size != 1 ? (kinds_2.size != 1 ? (kinds_3.size == 1 ? kinds_3.size : "0") : kinds_2[0])  : kinds_1[0]
end
def is_full_house(hand)
  kinds = hand.map{|e| e[0]}.uniq
  return kinds.size == 2 ? true : false
end
def is_four_of_kind(hand)
  kinds = hand.map{|e| e[0]}
  kinds_1 = kinds[1..kinds.size-1].uniq
  kinds_2 = kinds[0..kinds.size-2].uniq
  return kinds_1.size != 1 ? (kinds_2.size == 1 ? kinds_2[0] : "0") : kinds_1[0]
end
def is_straight(hand, rank_hash)
  i = 0
  while i < hand.size-2
    if(rank_hash[hand[i+1][0].chr] - rank_hash[hand[i][0].chr] != 1)
      return false
    end
    i += 1
  end
  return true
end
def is_flush(hand)
  suits = hand.map{|e| e[1]}.uniq
  return true if suits.size == 1
  return false

end




def decrypt(ciphertext, key)
  new_text = []
  i = 0
  while i < ciphertext.size
    new_text[i] = ciphertext[i] ^ key[i % key.length]
    i += 1
  end
  return new_text
end
def score(text)
  res = 0
  text.each do |x|
    res += 1 if x>=65 && x<=90
    res += 2 if x>=97 && x<=122
    res -= 10 if x < 32 || x == 127
  end
  return res
end
def task_59
  max_res = 0
  max_text = []
  ciphertext = []
     File.open('D:\\1Semestry\\RubyAga\\p059_cipher.txt', 'r') do |file|
       file.each_line do |x|
         ciphertext = (x.split(',').map! { |x| x.to_i })
       end
     end
  (97..122).each do |x|
    (97..122).each do |y|
      (97..122).each do |z|
        key = [x,y,z]
        if max_res < score(decrypt(ciphertext, key))
          max_res = score(decrypt(ciphertext, key))
          max_text = decrypt(ciphertext, key)
        end
      end
    end
  end

  p max_text.inject(:+)
end

def e_term(i)
  if i == 0
    return 2
  end
  if i % 3 == 2
    return i / 3 * 2 + 2
  end
  1
end
def task_65
  n = 1
  d = 0
  i = 99
  while i>=0
    temp = e_term(i)*n+d
    d = n
    n = temp
    i-=1
  end
  sum = 0
  while n!=0
    sum+=n%10
    n /=10
  end
  puts sum
end

def get_cods(filename)
  data = []
  File.open(filename, 'r') do |file|
    file.each_line { |x| data.push(x.delete("\n").to_i) }
  end
  data
end
def task_79
  res=""
  nums = [[],[],[],[],[],[],[],[],[],[]]
  cods = get_numbers('D:\\1Semestry\\RubyAga\\p079_keylog.txt')
  cods = cods.uniq.sort
  cods.each do |c|
    nums[(c.to_s)[0].to_i].push((c.to_s)[0].to_i) if !nums[(c.to_s)[0].to_i].include?((c.to_s)[0].to_i)
    nums[(c.to_s)[1].to_i].push((c.to_s)[1].to_i) if !nums[(c.to_s)[1].to_i].include?((c.to_s)[1].to_i)
    nums[(c.to_s)[2].to_i].push((c.to_s)[2].to_i) if !nums[(c.to_s)[2].to_i].include?((c.to_s)[2].to_i)

    nums[(c.to_s)[1].to_i].push((c.to_s)[0].to_i) if !nums[(c.to_s)[1].to_i].include?((c.to_s)[0].to_i)
    nums[(c.to_s)[2].to_i].push((c.to_s)[1].to_i) if !nums[(c.to_s)[2].to_i].include?((c.to_s)[1].to_i)
  end
  while nums.count>0
    nums.each do |n|
      nums.delete(n) if n.count==0
      if n.count == 1
        res += n[0].to_s
        buf = n[0]
        nums.each do |x|
          nums[nums.index(x)].delete(buf)
        end
      end
    end
  end
  p res
end

def get_numbers(filename)
  data = []
  File.open(filename, 'r') do |file|
    file.each_line { |x| data.push(x.delete("\n")) }
  end
  data
end
def roman_to_int(hash_num, num)
  i = 0
  res = 0

  while i < num.size
    res += hash_num[num[i].to_s]
    i+=1
  end
  res -= 2 if num.include?("IV") || num.include?("IX")

  res -= 20 if num.include?("XL") || num.include?("XC")

  res -= 200 if num.include?("CD") || num.include?("CM")
  return res
end
def int_to_roman(hash_num, number_to_convert)
  answer = ""
  hash_num.each do |num, numeral|
    if number_to_convert >= hash_num[num]
      num_numerals, number_to_convert = number_to_convert.divmod(hash_num[num])
      num_numerals.times { answer += hash_num.key(numeral) }
    end
  end
  answer
end
def roman_lenght(hash_num, num)
  digit_lenght = [0, 1, 2, 3, 2, 1, 2, 3, 4, 2]
  count = 0
  count += 2 if (num >= 4000)   #4000 is MMMM, which doesn't have a two-letter form #Compensate for this fact
  while num!=0
    count+= digit_lenght[num%10]
    num/=10
  end
  return  count
end
def task_89
  hash_num = Hash.new(0)
  hash_num["M"] = 1000
  hash_num["CM"] = 900
  hash_num["D"] = 500
  hash_num["CD"] = 400
  hash_num["C"] = 100
  hash_num["XC"] = 90
  hash_num["L"] = 50
  hash_num["XL"] = 40
  hash_num["X"] = 10
  hash_num["IX"] = 9
  hash_num["V"] = 5
  hash_num["IV"] = 4
  hash_num["I"] = 1
  result = 0
  numbers = get_numbers('D:\\1Semestry\\RubyAga\\p089_roman.txt')
  numbers.each do |x|
    result += x.size - roman_lenght(hash_num,roman_to_int(hash_num, x))
  end
  puts result
end

def square_digit_sum(n)
  res = 0
  while n!=0
    res += (n%10)**2
    n/=10
  end
  return res
end
def task_92
  res = 0
  (1..10_000_000).each do |n|
    next_iter = square_digit_sum(n)
    while next_iter!=1
      if next_iter == 89
        res+=1
        break
      end
      next_iter = square_digit_sum(next_iter)
    end
  end
  p res
end


class Class
  def my_attr_accessor(*attrs)
    attrs.each do |attr|
      define_method(attr) {instance_variable_get("@#{attr}")} # This is the getter
      define_method("#{attr}=") {|arg| instance_variable_set("@#{attr}", arg)}
    end
  end
end
class People
  my_attr_accessor(:name,:age)
  def initialize(name, age)
    @name = name
    @age = age
  end
end

def task_244

end


#CG

def ascii_art
  # Auto-generated code below aims at helping you parse
  # the standard input according to the problem statement.
  num = 26
  l = gets.to_i
  h = gets.to_i
  t = gets.chomp
  res = ""
  h.times do
    row = gets.chomp
    i = 0

    while i < t.size

      number = t[i].ord
      if number>=65 && number<=90
        number -= 65
      elsif number>=97 && number<=122
        number -= 97
      else
        number = 26
      end
      res += row[number*l..number*l+3] #www
      i += 1
    end
    res+="\n"
  end

  puts res
end

def thor
  STDOUT.sync = true # DO NOT REMOVE

  light_x, light_y, initial_tx, initial_ty = gets.split(" ").collect { |x| x.to_i }

  # game loop
  loop do
    remaining_turns = gets.to_i # The remaining amount of turns Thor can move. Do not remove this line.
    res = ""
    initial_ty-light_y <= 0 ? initial_ty - light_y == 0 ? res += "" : (res += "S"; initial_ty += 1) : (res += "N"; initial_ty-=1)
    initial_tx-light_x <= 0 ? initial_tx - light_x == 0 ? res += "" : (res += "E"; initial_tx += 1) : (res += "W"; initial_tx-=1)
    # Write an action using puts
    # To debug: STDERR.puts "Debug messages..."


    # A single line providing the move to be made: N NE E SE S SW W or NW
    puts res
  end

end

def chuck_norris
  # Auto-generated code below aims at helping you parse
  # the standard input according to the problem statement.
  res = ""
  message = gets.chomp
  i = 0
  byte_str = message.bytes.map { |j|"%07b" %j }.join()
  while i < byte_str.size
    res += byte_str[i] == "1" ? "0 " : "00 "
    res += "0"
    while byte_str[i] == byte_str[i+1]
      res += "0"
      i += 1
    end
    res += " "
    i += 1
  end
  puts res.strip
end

def str_to_num(command)
  if command == 'P'
    return 2
  elsif command == 'R'
    return 0
  elsif command == 'C'
    return 4
  elsif command == 'S'
    return 1
  elsif command == 'L'
    return 3
  end
  return -1
end
def rpcls
  n = gets.to_i
  commands = []
  n.times do
    commands.push(gets.delete("\n"))
  end
  lose_str = Array.new(n+1,"")
  while commands.size>1
    i = 0
    while i+1 < commands.length
      x = (str_to_num(commands[i].split(" ")[1]) - str_to_num(commands[i+1].split(" ")[1])) % 5

      if x == 1
        lose_str[commands[i].split(" ")[0].to_i] += commands[i+1].split(" ")[0] + " "
        commands[i+1] = ""
      elsif x == 2
        lose_str[commands[i].split(" ")[0].to_i] += commands[i+1].split(" ")[0] + " "
        commands[i+1] = ""
      elsif x == 3
        lose_str[commands[i+1].split(" ")[0].to_i] += commands[i].split(" ")[0] + " "
        commands[i] = ""
      elsif x == 4
        lose_str[commands[i+1].split(" ")[0].to_i] += commands[i].split(" ")[0] + " "
        commands[i] = ""
      else
        if commands[i].split(" ")[0].to_i < commands[i+1].split(" ")[0].to_i
          lose_str[commands[i].split(" ")[0].to_i] += commands[i+1].split(" ")[0] + " "
          commands[i+1] = ""
        else
          lose_str[commands[i+1].split(" ")[0].to_i] += commands[i].split(" ")[0] + " "
          commands[i] = ""
        end
      end
      i+=2
    end
    commands.delete("")
  end
  puts commands[0].split(" ")[0]
  puts lose_str[commands[0].split(" ")[0].to_i].chop
end

def forest_fire
  STDOUT.sync = true # DO NOT REMOVE
  # Send your available units to put out those fires! Watch out for water supplies!

  l = gets.to_i # Size of forest map
  forest = Array.new(l){Array.new(l,0)}
  water = gets.to_i # Total amount of water available
  # game loop
  loop do
    n = gets.to_i # Amount of fires
    n.times do
      # fire_x: X coordinate of fire
      # fire_y: Y coordinate of fire5
      fire_x, fire_y = gets.split(" ").collect { |x| x.to_i }
      forest[fire_y][fire_x] = 1


    end
    p forest
    # Write an action using puts
    # To debug: STDERR.puts "Debug messages..."

    puts "" if n == 1
  end
end

def batman
  STDOUT.sync = true # DO NOT REMOVE
  # Auto-generated code below aims at helping you parse
  # the standard input according to the problem statement.

  # w: width of the building.
  # h: height of the building.

  w, h = gets.split(" ").collect { |x| x.to_i }
  n = gets.to_i # maximum number of turns before game over.
  x0, y0 = gets.split(" ").collect { |x| x.to_i }
  x1 = 0
  y1 = 0
  x2 = w - 1
  y2 = h - 1
  # game loop
  loop do
    bomb_dir = gets.chomp # the direction of the bombs from batman's current location (U, UR, R, DR, D, DL, L or UL)

    # Write an action using puts
    # To debug: STDERR.puts "Debug messages..."
    if bomb_dir.include?("U")
      y2 = y0 - 1
    elsif bomb_dir.include?("D")
      y1 = y0 + 1
    end

    if bomb_dir.include?("L")
      x2 = x0 - 1
    elsif bomb_dir.include?("R")
      x1 = x0 + 1
    end

    x0 = x1 + (x2 - x1) / 2
    y0 = y1 + (y2 - y1) / 2

    # the location of the next window Batman should jump to.
    puts x0.to_s + " " + y0.to_s
  end
end

def there_is_no_spoon
  STDOUT.sync = true # DO NOT REMOVE
  # Don't let the machines win. You are humanity's last hope...

  width = gets.to_i # the number of cells on the X axis
  height = gets.to_i # the number of cells on the Y axis
  field = Array.new(height){Array.new(width, 0)}
  i = 0

  height.times do
    j = 0
    line = gets.chomp # width characters, each either 0 or .
    arr_line = line.chars
    arr_line.each do |x|
      field[i][j] = 0 if x == "."
      field[i][j] = 1 if x == "0"
      j+=1
    end
    i+=1
  end

  i = 0
  while i < height
    j = 0
    while j < width
      res = ""
      if  field[i][j].to_i == 0
        j+=1
        next
      end
      res = j.to_s + " " + i.to_s + " "
      k = j
      if k < width-1
        while(k < width-1)
          if field[i][k+1] == 1
            res += (k+1).to_s + " " + i.to_s + " "
            break
          end
          k += 1
          res += "-1 -1 " if k == width-1
        end
      else
        res += "-1 -1 "
      end
      k = i
      if k < height-1
        while(k < height-1)
          if field[k+1][j] == 1
            res += j.to_s + " " + (k+1).to_s + " "
            break
          end
          k += 1
          res += "-1 -1 " if k == height-1
        end
      else
        res += "-1 -1 "
      end
      j += 1
      puts res.strip
    end
    i += 1
  end
end

def rpn
  # Auto-generated code below aims at helping you parse
  # the standard input according to the problem statement.

  n = gets.to_i
  inputs = gets.split(" ")
  stack = []
  for i in 0..(n-1)

    if inputs[i][0].ord > 44 && inputs[i][0].ord < 58
      stack.push(inputs[i])
    end
    if inputs[i] == "ADD"
      if(stack.size > 1)
        res = (stack.delete_at(-2).to_i + stack.delete_at(-1).to_i).to_s
        stack.push(res)
      else
        stack.push("ERROR")
        break
      end
    end
    if inputs[i] == "SUB"
      if(stack.size > 1)
        res = (stack.delete_at(-2).to_i - stack.delete_at(-1).to_i).to_s
        stack.push(res)
      else
        stack.push("ERROR")
        break
      end
    end
    if inputs[i] == "MUL"
      if(stack.size > 1)
        res = (stack.delete_at(-2).to_i * stack.delete_at(-1).to_i).to_s
        stack.push(res)
      else
        stack.push("ERROR")
        break
      end
    end
    if inputs[i] == "DIV"
      if(stack.size > 1)
        res = "ERROR"
        a = stack.delete_at(-2).to_i
        b = stack.delete_at(-1).to_i
        if b != 0
          res = (a / b).to_s
        end
        stack.push(res)
      else
        stack.push("ERROR")
        break
      end
    end
    if inputs[i] == "MOD"
      if(stack.size > 1)
        res = "ERROR"
        a = stack.delete_at(-2).to_i
        b = stack.delete_at(-1).to_i
        if b != 0
          res = (a % b).to_s
        end
        stack.push(res)
      else
        stack.push("ERROR")
        break
      end
    end
    if inputs[i] == "SWP"
      if(stack.size > 1)
        a = stack.delete_at(-2).to_i
        b = stack.delete_at(-1).to_i
        stack.push(b)
        stack.push(a)
      else
        stack.push("ERROR")
        break
      end
    end
    if inputs[i] == "ROL"
      k = stack.delete_at(-1).to_i
      if k > stack.size
        stack.clear()
        stack.push("ERROR")
      else
        rol_stack = []
        j = stack.size-k
        (j..stack.size-1).each do |x|
          rol_stack.push(stack.delete_at(j))
        end
        rol_stack.push(rol_stack.delete_at(0))
        stack.push(rol_stack)
      end

    end
    if inputs[i] == "POP"
      if(stack.size > 0)
        stack.delete_at(-1)
      else
        stack.push("ERROR")
        break
      end
    end
    if inputs[i] == "DUP"
      if(stack.size > 0)
        stack.push(stack[-1])
      else
        stack.push("ERROR")
        break
      end
    end
  end

  # Write an answer using puts
  # To debug: STDERR.puts "Debug messages..."
  puts stack.join(" ")
end

module A
  def a
    puts "A"
  end
end
class Test1
  include(A)
  def test
    puts "hello"
  end
end
test = Test1.new


