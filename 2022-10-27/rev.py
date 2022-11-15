def rev(l) : return ([] if len(l) == 0 else rev(l[1:]) + [l[0]])    
print(rev("1234"))


def rev2(l):
  r = []
  while True:
    if len(l) == 0:
      break
    r = [l[0]]+r
    l = l[1:]
  return r

print(rev2("1234"))    



