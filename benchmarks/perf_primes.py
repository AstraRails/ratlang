TARGET = 500
ITERATIONS = 100


def is_prime(value: int) -> bool:
    if value < 2:
        return False
    if value == 2:
        return True
    if value % 2 == 0:
        return False
    check = 3
    while check * check <= value:
        if value % check == 0:
            return False
        check += 2
    return True


def nth_prime(idx: int) -> int:
    if idx <= 0:
        return 0
    found = 0
    candidate = 1
    while found < idx:
        candidate += 1
        if is_prime(candidate):
            found += 1
    return candidate


def checksum(iterations: int, target: int) -> int:
    total = 0
    for _ in range(iterations):
        total += nth_prime(target)
    return total


if __name__ == "__main__":
    total = checksum(ITERATIONS, TARGET)
    print(f"python primes checksum: {total}")
