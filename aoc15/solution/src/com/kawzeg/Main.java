package com.kawzeg;

import java.util.*;

public class Main {
    private static int sayNth(int n, LinkedList<Integer> starting) {
        int[] lastIndex = new int[n];
        for (int i = 0; i < starting.size(); i++) {
            lastIndex[starting.get(i)] = i+1;
        }
        int last = starting.getLast();
        for (int i = starting.size(); i < n; i++) {
            int next = lastIndex[last] == 0 ? 0 : i - lastIndex[last];
            lastIndex[last] = i;
            last = next;
        }
        return last;
    }

    public static void main(String[] args) {
        LinkedList<Integer> input = new LinkedList<>(Arrays.asList(12, 20, 0, 6, 1, 17, 7));
        System.out.printf("Part 1: %d%n", sayNth(2020, input));
        System.out.printf("Part 2: %d%n", sayNth(30000000 /* lol */, input));
    }
}
