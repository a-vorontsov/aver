import random

s = 100000

with open("examples/sorting.av", "w") as f:
    f.write(
        "func bubbleSort(list int[]) void {\n    let n = " + str(s) + ";\n")
    f.write("""    let i = 0;
    while (i < n - 1) {
        let j = 0;
        while (j < (n - i - 1)) {
            if (list[j] > list[j+1]) {
                let temp = list[j];
                list[j] = list[j+1];
                list[j+1] = temp;
            } else {
                pass;
            }
            j = j + 1;
        }
        i = i + 1;
    }
}\nfunc main() void {\n    let list = [""")
    f.write(",".join([str(random.randint(0, 1000)) for _ in range(s)]))
    f.write("""];\n    bubbleSort(list);
}""")
