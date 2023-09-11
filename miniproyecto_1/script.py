# import bcrypt

# # example password
# password = 'admin'
    
# # Hashing the password
# hash = bcrypt.hashpw(password.encode('utf-8'), bcrypt.gensalt())
  
# print(hash.decode('utf-8'))

# userPassword =  input('Insert password: ')
  
# result = bcrypt.checkpw(userPassword.encode('utf-8'), hash)
  
# print(result)


#RSA
# a = [8,5,12,12,15]
# n = 323
# e = 5
# d = 173
# b = []

# for i in a:
#     b.append((i**e)%n)
# print(b)

# c = []

# for i in b:
#     c.append((i**d)%n)
# print(c)

# print(c == a)


# import pandas as pd
# data = [
#     [0.0, 48, 17066],
#     [0.5, 38, 14464],
#     [1.5, 5, 788],
#     [4.7, 2, 163],
# ]

# df = pd.DataFrame(data, columns=['alcohol', 'frec_presente', 'frec_ausente'])
# new_data = []

# for _, row in df.iterrows():
#     frec_presente = row['frec_presente']
#     frec_ausente = row['frec_ausente']
#     alcohol = row['alcohol']

#     for _ in range(int(frec_presente)):
#         new_data.append([alcohol, 1])

#     for _ in range(int(frec_ausente)):
#         new_data.append([alcohol, 0])

# new_df = pd.DataFrame(new_data, columns=['alcohol', 'malformacion'])

# new_df.to_excel("data.xlsx")