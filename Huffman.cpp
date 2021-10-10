#include "Huffman.h"

#include <algorithm>
#include <istream>
#include <iostream>
#include <ostream>
#include <vector>
#include <queue>
#include <memory>

using namespace Arch;

enum Error {
    OK = 0,
    WRONG_ARGUMENTS,
    READ_ERROR,
    MALFORMED_INPUT
    // You can add here yours errors.
};

const int BlOCK_SIZE = (1 << 14), BITS_IN_CHAR = 8,
    ALPHA = 256, BUF_SIZE = (1 << 12);

class BytesStream {

    char lbuf[BUF_SIZE];
    size_t lused = 0;

    uint8_t sbuf = 0;
    size_t sused = 0;

    std::ostream *out;

public:
    BytesStream(std::ostream *out_): out(out_) {}

    void sflush() {
        lbuf[lused] = sbuf;
        ++lused;
        sbuf = 0;
        sused = 0;
        if (lused == BUF_SIZE)
            flush();
    }

    void flush() {
        if (sused > 0)
            sflush();

        if (lused > 0) {
            out->write(lbuf, lused);
            lused = 0;
        }
    }

    void write(uint8_t data, size_t len) {
        sbuf |= (data << sused);
        if (len + sused >= BITS_IN_CHAR) {
            auto tmp = sused;
            sflush();
            sbuf = (data >> (BITS_IN_CHAR - tmp));
            sused = len - (BITS_IN_CHAR - tmp);
        }
        else {
            sused += len;
        }
    }
};

int x = 0;

class Trie {
public:
    class Node {
    public:
        static inline int nodeCount = 0;

        std::shared_ptr<Node> children[2] = {nullptr, nullptr};
        bool terminal = false;
        uint8_t symbol;
        int nodeId;

        Node() {
            nodeId = nodeCount++;
        }

        Node(bool term, uint8_t symb): Node() {
            terminal = term;
            symbol = symb;
        }

        void destroy() {
            children[0].reset();
            children[1].reset();
        }
    };

    using NodePtr = std::shared_ptr<Node>;

    uint8_t len[ALPHA];
    uint32_t code[ALPHA];
    NodePtr root;

    void dfs(NodePtr node, int height = 0) {
        if (node->terminal) {
            len[node->symbol] = height;
        } else {
            for (int i = 0; i < 2; i++) {
                if (node->children[i]) {
                    dfs(node->children[i], height + 1);
                }
            }
        }
    }

private:
    struct QueueComparator {
        bool operator()(const std::pair<uint16_t, NodePtr> &x, const std::pair<uint16_t, NodePtr> &y) {
            return x.first == y.first ? x.second->nodeId > y.second->nodeId : x.first > y.first;
        }
    };

public:
    void buildTrie(uint32_t* count) {
        std::priority_queue<std::pair<uint16_t, NodePtr>,
            std::vector<std::pair<uint16_t, NodePtr>>,
            QueueComparator> q;

        for (int i = 0; i < ALPHA; i++) {
            NodePtr newNode(new Node(true, static_cast<uint8_t>(i)));
            q.push(std::make_pair(count[i], newNode));
        }

        while (q.size() > 1) {
            auto leftChild = q.top();
            q.pop();
            auto rightChild = q.top();
            q.pop();
            NodePtr newNode(new Node());
            newNode->children[0] = leftChild.second;
            newNode->children[1] = rightChild.second;
            q.push(std::make_pair(leftChild.first + rightChild.first, newNode));
        }

        root = q.top().second;
        dfs(root);
    }

    void buildCanonicalCode() {
        std::pair<uint8_t, unsigned char> ar[ALPHA];
        for (int i = 0; i < ALPHA; i++) {
            ar[i].first = len[i];
            ar[i].second = static_cast<unsigned char>(i);
        }
        sort(ar, ar + ALPHA);

        code[ar[0].second] = 0;
        for (int i = 1; i < ALPHA; i++) {
            auto& cur = code[ar[i].second];
            cur = code[ar[i - 1].second];
            ++cur;
            cur = (cur << (ar[i].first - ar[i - 1].first));
        }

        for (int i = 0; i < ALPHA; i++) {
            uint64_t reversedCode = 0;
            for (uint8_t j = 0; j < len[i]; j++) {
                if ((code[i] >> j) & 1)
                    reversedCode |= (1 << (len[i] - 1 - j));
            }
            code[i] = reversedCode;
        }
    }

    void buildCanonicalTrie() {
        if (root)
            root->destroy();
        else
            root.reset(new Node());

        for (int i = 0; i < ALPHA; i++) {
            NodePtr node = root;
            for (int j = 0; j < len[i]; ++j) {
                bool bit = ((code[i] >> j) & 1);
                if (!node->children[bit])
                    node->children[bit].reset(new Node());
                node = node->children[bit];
            }
            node->symbol = static_cast<uint8_t>(i);
            node->terminal = true;
        }
    }

    class Walker {
        NodePtr node, root;
        BytesStream* bstr;
    public:
        Walker(NodePtr root_, BytesStream* bstr_):
            node(root_),
            root(root_),
            bstr(bstr_) {}

        bool move(bool bit) {
            if (!node->children[bit])
                return false;
            node = node->children[bit];
            if (node->terminal) {
                bstr->write(node->symbol, 8);
                node = root;
            }
            return true;
        }
    };

    Walker getWalker(BytesStream* btr) {
        return Walker(root, btr);
    }
};

void compressBlock(std::ostream *out, char* block, size_t size, Trie &trie) {
    uint32_t count[ALPHA];
    std::fill(count, count + ALPHA, 0);

    for (size_t i = 0; i < size; ++i)
        ++count[static_cast<uint8_t>(block[i])];

    trie.buildTrie(count);
    trie.buildCanonicalCode();

    out->write(reinterpret_cast<char*>(trie.len), sizeof(uint8_t) * ALPHA);

    uint64_t bitsCount = 0;
    for (size_t i = 0; i < size; ++i) {
        bitsCount += trie.len[static_cast<unsigned char>(block[i])];
    }

    out->write(reinterpret_cast<char*>(&bitsCount), sizeof(uint64_t));

    BytesStream bstr(out);

    for (size_t i = 0; i < size; ++i) {
        unsigned char c = static_cast<unsigned char>(block[i]);
        uint64_t code = trie.code[c];
        uint8_t len = trie.len[c];

        while (len > 0) {
            uint8_t cur = (len > 8 ? 8 : len);
            bstr.write(code, cur);
            code >>= cur;
            len -= cur;
        }
    }

    bstr.flush();
}

int Arch::compress(std::istream *in, std::ostream *out) {
    if (in == nullptr || out == nullptr) {
        return Error::WRONG_ARGUMENTS;
    }

    char block[BlOCK_SIZE];

    Trie trie;
    
    while (in->good()) {
        in->read(block, BlOCK_SIZE);
        compressBlock(out, block, in->gcount(), trie);
    }

    if (in->bad())
        return Error::READ_ERROR;

    return Error::OK;
}

int Arch::decompress(std::istream *in, std::ostream *out) {
    if (in == nullptr || out == nullptr)
        return Error::WRONG_ARGUMENTS;

    Trie trie;
    BytesStream bstr(out);
    char buf[BUF_SIZE];

    while (in->good()) {
        in->read(reinterpret_cast<char*>(trie.len), sizeof(uint8_t) * ALPHA);

        for (int i = 0; i < ALPHA; i++) {
            if (trie.len[i] > 30)
                return Error::MALFORMED_INPUT;
        }

        uint64_t bitsCount;
        in->read(reinterpret_cast<char*>(&bitsCount), sizeof(uint64_t));

        trie.buildCanonicalCode();
        trie.buildCanonicalTrie();

        auto walker = trie.getWalker(&bstr);

        while (bitsCount > 0 && in->good()) {
            uint64_t currentBufferSize = std::min(static_cast<uint64_t>(BUF_SIZE), (bitsCount + 7) >> 3);
            in->read(buf, currentBufferSize);
            if (in->fail())
                return Error::MALFORMED_INPUT;
            for (uint64_t i = 0; i < currentBufferSize; i++) {
                for (int j = 0; j < 8 && bitsCount > 0; ++j, --bitsCount) {
                    if (!walker.move((buf[i] >> j) & 1))
                        return Error::MALFORMED_INPUT;
                }
            }
        }

        if (bitsCount > 0)
            return Error::MALFORMED_INPUT;

        if (in->bad())
            return Error::READ_ERROR;
    }

    bstr.flush();

    return Error::OK;
}