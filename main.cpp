#include <iostream>
#include <utility>
#include <vector>
#include <algorithm>
#include <string>
#include <sstream>
#include <bitset>
#include <cmath>
#include <unordered_set>

#define DEBUG 0

using namespace std;

class rule {
public:
    string hs;
    string lhs;
    vector<string> rhs;

    // parse string to lhs and rhs
    void parseHs() {
        hs.erase(std::remove(hs.begin(), hs.end(), ' '), hs.end());
        size_t pos = hs.find("->");
        lhs = hs.substr(0, pos);
        string rhsString = hs.substr(pos + 2);
        stringstream sss(rhsString);
        string token;
        while (getline(sss, token, '|'))
            rhs.push_back(token);
    }

    void print() {
        cout << lhs << " -> ";
        for (auto i = 0; i < rhs.size(); i++) {
            if (i == rhs.size() - 1) {
                cout << rhs[i];
            } else {
                cout << rhs[i] << " | ";
            }
        }
    }


    rule() {
        getline(std::cin, hs);
        parseHs();
    }

    explicit rule(string hs) {
        this->hs = std::move(hs);
        parseHs();
    }

    ~rule() {
        vector<string>().swap(rhs);
    }
};

bool operator==(const rule &x, const rule &y) {
    return x.hs == y.hs && x.lhs == y.lhs && x.rhs == y.rhs;
}

bool operator<(const rule &x, const rule &y) {
    if (x.hs != y.hs) return x.hs < y.hs;
    if (x.lhs != y.lhs) return x.lhs < y.lhs;
    return x.rhs < y.rhs;
}

class grammar {
public:
    vector<pair<rule, bool>> rules;
    vector<string> nullVar,  allVar;
    vector<string> newProduction;
    vector<string> accessibleVar;
    vector<vector<int>> unitProPos;

    void print() {
        for (auto i: rules) {
            i.first.print();
            cout << endl;
        }
    }

    // remove nullable variable
    void rmNullVar() {
        while (findNull()) {
            // iterate in null variable
            for (const auto &nvar: nullVar) {
                // iterate in all grammar rules
                for (auto &gram: rules) {
                    // iterate in rule rhs and create new production when removing null
                    newProduction.clear();
                    newProduction.shrink_to_fit();
                    for (const auto &unit: gram.first.rhs) {
                        string u{unit};
                        int count{};
                        // replace all null variable with $ and count number of it
                        while (u.find(nvar) != string::npos) {
                            u.replace(u.find(nvar), nvar.length(), "$");
                            count++;
                        }
                        // calculate number of bit for creating combination
                        count = ceil(sqrt(count));
                        // create all combination with assigning null to variable
                        for (auto i = 0; i < pow(count, 2); i++) {
                            bitset<16> b(i);
                            string bs = b.to_string(), up{};
                            int index{15};
                            for (auto j: u) {
                                if (j == '$' && bs[index] == '0') {
                                    index--;
                                    continue;
                                } else if (j == '$' && bs[index] == '1') {
                                    index--;
                                }
                                up += j;
                            }
                            // replace all $ with null variable
                            while (up.find('$') != string::npos) {
                                up.replace(up.find('$'), 1, nvar);
                            }
                            if (up.empty()) up = "$";
                            newProduction.push_back(up);
                        }
                    }
                    // add new production to rhs of rule
                    gram.first.rhs.insert(gram.first.rhs.end(), newProduction.begin(), newProduction.end());
                    // remove duplicate from rhs
                    unordered_set<string> rmdp;
                    auto end = std::remove_copy_if(gram.first.rhs.begin(), gram.first.rhs.end(), gram.first.rhs.begin(),
                                                   [&rmdp](string const &i) { return !rmdp.insert(i).second; });
                    gram.first.rhs.erase(end, gram.first.rhs.end());
                }
            }
            int ind{};
            for (auto &i: rules) {
                for (auto &j: i.first.rhs) {
                    // set flag of rule that their null production is removed
                    if (j == "#") {
                        i.second = true;
                    } else if (j == "$") {
                        j = "#";
                    }
                }
                if (i.second)
                    i.first.rhs.erase(remove(i.first.rhs.begin(), i.first.rhs.end(), "#"), i.first.rhs.end());
                // remove rule with empty rhs
                if (i.first.rhs.empty())
                    rules.erase(rules.begin() + ind, rules.begin() + ind + 1);
                ind++;
            }
            if (DEBUG) {
                print();
                cout << "======================================\n";
            }
        }
    }

    // search the grammar for nullable variable and if the function find one return true else return false
    bool findNull() {
        nullVar.clear();
        nullVar.shrink_to_fit();
        for (auto &i: rules) {
            for (auto &j: i.first.rhs) {
                if (j == "#" && !i.second) {
                    nullVar.push_back(i.first.lhs);
                } else if (j == "#" && i.second) {
                    // if the null production once removed it will not be removed tiwce just ignoring it
                    i.first.rhs.erase(remove(i.first.rhs.begin(), i.first.rhs.end(), "#"), i.first.rhs.end());
                }
            }
        }
        return !nullVar.empty();
    }

    // remove unit production
    void rmUnitPro() {
        while (findUnitPro()) {
            for (const auto &i: unitProPos) {
                // remove unit production
                rules[i[0]].first.rhs.erase(rules[i[0]].first.rhs.begin() + i[1]);
                // replace unit production with its rules
                rules[i[0]].first.rhs.insert(rules[i[0]].first.rhs.end(), rules[i[2]].first.rhs.begin(),
                                             rules[i[2]].first.rhs.end());
                // remove duplicate production
                unordered_set<string> rmdp;
                auto end = std::remove_copy_if(rules[i[0]].first.rhs.begin(), rules[i[0]].first.rhs.end(),
                                               rules[i[0]].first.rhs.begin(),
                                               [&rmdp](string const &i) { return !rmdp.insert(i).second; });
                rules[i[0]].first.rhs.erase(end, rules[i[0]].first.rhs.end());
            }
            if (DEBUG) {
                print();
                cout << "======================================\n";
            }
        }
    }

    // search the grammar for unit production and if the function find one return true else return false
    bool findUnitPro() {
        unitProPos.clear();
        unitProPos.shrink_to_fit();
        int x{}, y{}, z{};
        vector<int> position;
        for (const auto &i: rules) {
            for (const auto &j: rules) {
                for (const auto &k: i.first.rhs) {
                    // find the position of unit porduction and the position of rule to replace it
                    if (k == j.first.lhs) {
                        position.clear();
                        position.shrink_to_fit();
                        position.push_back(x);
                        position.push_back(z);
                        position.push_back(y);
                        unitProPos.push_back(position);
                    }
                    z++;
                }
                z = 0;
                y++;
            }
            y = 0;
            x++;
        }
        return !unitProPos.empty();
    }

    void rmUselessPro() {
        while(findUselessPro()) {
            vector<string> uselessVar;
            vector<pair<rule, bool>> accessibleRule;
            for (const auto &i: rules)
                allVar.push_back(i.first.lhs);
            // find accessible variable
            accessibleVar.push_back(rules[0].first.lhs);
            for(auto &i: rules) {
                for(auto &j: i.first.rhs) {
                    for(auto &k: allVar) {
                        if(i.first.lhs != k && j.find(k) != string::npos) {
                            accessibleVar.push_back(k);
                        }
                    }
                }
            }
            // remove duplicate
            unordered_set<string> access;
            auto end = std::remove_copy_if(accessibleVar.begin(), accessibleVar.end(), accessibleVar.begin(),
                                           [&access](string const &i) { return !access.insert(i).second; });
            accessibleVar.erase(end, accessibleVar.end());
            // add non accessible var to vector
            for (const auto &i: rules)
                if (find(accessibleVar.begin(), accessibleVar.end(), i.first.lhs) == accessibleVar.end())
                    uselessVar.push_back(i.first.lhs);
            // remove not accessible rule
            for (const auto &i: rules) {
                for (const auto &j: accessibleVar) {
                    if (i.first.lhs == j) {
                        accessibleRule.emplace_back(i.first, false);
                        break;
                    }
                }
            }
            rules.swap(accessibleRule);
            accessibleRule.clear();
            vector<pair<rule, bool>>().swap(accessibleRule);
            accessibleVar.clear();
            vector<string>().swap(accessibleVar);
            // remove production contain useless variable
            for (const auto &i: uselessVar) {
                for (auto &j: rules) {
                    for (auto &k: j.first.rhs) {
                        if (k.find(i) != string::npos) {
                            j.first.rhs.erase(remove(j.first.rhs.begin(), j.first.rhs.end(), k), j.first.rhs.end());
                        }
                    }
                }
            }
            uselessVar.clear();
            // find rules without terminal
            rules[0].second = true;
            for (auto &i: rules) {
                for (auto &j: i.first.rhs) {
                    if (j.size() == 1) {
                        i.second = true;
                        break;
                    }
                }
                if (!i.second)
                    uselessVar.push_back(i.first.lhs);
            }
            allVar.clear();
            for (const auto &i: rules)
                if (i.second)
                    allVar.push_back(i.first.lhs);
            // confirm that the rules of previous section should be removed
            for (auto &i: rules) {
                bool flag{false};
                if (!i.second) {
                    for (const auto &j: i.first.rhs) {
                        for (const auto &k: allVar) {
                            if (j.find(k) != string::npos && j.find(i.first.lhs) == string::npos) {
                                i.second = true;
                                flag = true;
                                break;
                            }
                        }
                        if (flag)
                            break;
                    }
                }
            }
            // remove useless rule
            for (const auto &i: rules) {
                if (i.second) {
                    accessibleRule.emplace_back(i.first, false);
                }
            }
            rules.swap(accessibleRule);
            accessibleRule.clear();
            vector<pair<rule, bool>>().swap(accessibleRule);
            // remove production contain useless variable
            for (const auto &i: uselessVar) {
                for (auto &j: rules) {
                    for (auto &k: j.first.rhs) {
                        if (k.find(i) != string::npos) {
                            j.first.rhs.erase(remove(j.first.rhs.begin(), j.first.rhs.end(), k), j.first.rhs.end());
                        }
                    }
                }
            }
            if (DEBUG) {
                print();
                cout << "======================================\n";
            }
        }
    }

    bool findUselessPro() {
        bool flag = false;
        for(const auto &i: rules) {
            for(const auto &j: i.first.rhs) {
                if(j.size() == 3) {
                    flag = true;
                    break;
                }
            }
            if(flag)
                break;
        }
        return flag;
    }

    ~grammar() {
        rules.clear();
        vector<pair<rule, bool>>().swap(rules);
        nullVar.clear();
        vector<string>().swap(nullVar);
        newProduction.clear();
        vector<string>().swap(newProduction);
        unitProPos.clear();
        vector<vector<int>>().swap(unitProPos);
    }
} gmr;

class parser {
public:
    struct chomsky {
        string lhs, rhs;
    };
    vector<rule> parse_grammar;
    vector<chomsky> cfg;

    void cnf() {
        vector<rule> terminalToVariable;
        // create name for new variable
        vector<string> name;
        for (auto i = 'A'; i <= 'Z'; i++)
            name.push_back("<" + string(1, i) + ">");
        for (auto &i: parse_grammar)
            name.erase(remove(name.begin(), name.end(), i.lhs), name.end());
        // find all terminal and make variable for them
        for (const auto &i: parse_grammar) {
            for (const auto &j: i.rhs) {
                if (j.size() != 1) {
                    for (auto k = 0; k < j.length(); k++) {
                        char c = j[k];
                        if (c == '<') {
                            k += 2;
                        } else {
                            string lhs{}, hs;
                            for (auto &k: terminalToVariable) {
                                if (k.rhs[0] == string(1, c)) {
                                    lhs = k.lhs;
                                    break;
                                }
                            }
                            if (lhs.empty()) {
                                hs = name[0] + " -> " + string(1, c);
                                name.erase(name.begin(), name.begin() + 1);
                            } else {
                                hs = lhs + " -> " + string(1, c);
                            }
                            rule r(hs);
                            terminalToVariable.push_back(r);
                        }
                    }
                }
            }
        }
        // remove duplicate
        sort(terminalToVariable.begin(), terminalToVariable.end());
        auto last = unique(terminalToVariable.begin(), terminalToVariable.end());
        terminalToVariable.erase(last, terminalToVariable.end());
        // add new rules to grammar
        parse_grammar.insert(parse_grammar.end(), terminalToVariable.begin(), terminalToVariable.end());
        // change terminal in production to variable
        for (auto &i: parse_grammar) {
            for (auto &j: i.rhs) {
                if (j.size() > 1) {
                    for (auto &k: terminalToVariable) {
                        while (j.find(k.rhs[0]) != string::npos) {
                            j.replace(j.find(k.rhs[0]), 1, "$");
                        }
                        while (j.find("$") != string::npos) {
                            j.replace(j.find("$"), 1, k.lhs);
                        }
                    }
                }
            }
        }
        terminalToVariable.clear();
        vector<rule>().swap(terminalToVariable);
        // create name for new variable
        for (auto i = 'A'; i <= 'Z'; i++)
            name.push_back("<" + string(1, i) + ">");
        for (auto &i: parse_grammar)
            name.erase(remove(name.begin(), name.end(), i.lhs), name.end());
        // reduce the production with more than 2 variable
        vector<rule> variable;
        while (true) {
            int index{0};
            bool flag{false};
            for (auto &i: parse_grammar) {
                for (auto &j: i.rhs) {
                    if (j.size() > 6) {
                        flag = true;
                        string lhs{}, hs{};
                        for (auto &k: variable)
                            if (k.rhs[0] == j.substr(3)) {
                                lhs = k.lhs;
                                break;
                            }
                        if (lhs.empty()) {
                            hs = name[0] + " -> " + j.substr(3);
                            j.replace(3, j.substr(3).length(), name[0]);
                            name.erase(name.begin(), name.begin() + 1);
                            rule r(hs);
                            variable.push_back(r);
                            index++;
                        } else {
                            j.replace(3, j.substr(3).length(), lhs);
                        }
                    }
                }
            }
            if (!flag)
                break;
            parse_grammar.insert(parse_grammar.end(), variable.end() - index, variable.end());
        }
        // remove '<' and '>'
        for (auto &i: parse_grammar) {
            i.lhs.erase(remove(i.lhs.begin(), i.lhs.end(), '<'), i.lhs.end());
            i.lhs.erase(remove(i.lhs.begin(), i.lhs.end(), '>'), i.lhs.end());
            for (auto &j: i.rhs) {
                j.erase(remove(j.begin(), j.end(), '<'), j.end());
                j.erase(remove(j.begin(), j.end(), '>'), j.end());
            }
            for (auto j = 0; j < i.rhs.size(); j++) {
                chomsky c{.lhs=i.lhs, .rhs=i.rhs[j]};
                cfg.push_back(c);
            }
        }
        if (DEBUG) {
            print();
//            cout << "======================================\n";
//            for (auto &i: cfg) {
//                cout << i.lhs << " -> " << i.rhs << endl;
//            }
            cout << "======================================\n";
        }
    }

    void cyk(string language) {
        int size = language.length();
        vector<vector<vector<string>>> table(size, vector<vector<string>>(size, vector<string>(0)));

        // filling the first row
        for (auto i = 0; i < size; i++) {
            table[0][i] = getTerminal(language[i]);
        }

        // filling the other rows
        for (int length = 1; length < size; length++) {
            for (int i = 0; i < size - length; i++) {
                int j = i + length;
                for (int k = i; k < j; k++) {
                    auto new_variables = getVariable(table[k - i][i], table[j - k - 1][k + 1]);
                    table[length][i].insert(table[length][i].end(), new_variables.begin(), new_variables.end());
                }
            }
        }

        bool flag{false};
        for (auto &i: table[size - 1][0]) {
            if (i == cfg[0].lhs) {
                flag = true;
                break;
            }
        }
        if (flag)
            cout << "Accepted\n";
        else
            cout << "Rejected\n";
    }

    vector<string> getVariable(vector<string> x, vector<string> y) {
        vector<string> uni;
        for (auto &i: x) {
            for (auto &j: y) {
                uni.push_back(i + j);
            }
        }
        vector<string> lhs;
        for (auto &i: cfg) {
            for (auto &j: uni) {
                if (i.rhs == j) {
                    lhs.push_back(i.lhs);
                }
            }
        }
        return lhs;
    }

    vector<string> getTerminal(char c) {
        vector<string> lhs;
        for (auto &i: cfg) {
            if (i.rhs == string(1, c)) {
                lhs.push_back(i.lhs);
            }
        }
        return lhs;
    }

    void print() {
        for (auto &item: parse_grammar) {
            item.print();
            cout << endl;
        }
    }

    explicit parser(string language) {
        gmr.rmNullVar();
        gmr.rmUnitPro();
        gmr.rmUselessPro();
        for (const auto &i: gmr.rules)
            parse_grammar.push_back(i.first);
        cnf();
        cyk(std::move(language));
    }
};

int main() {

    if (DEBUG) {
        const string g[]{"<E> -> <E>+<T> | <E>-<T> | <T>", "<T> -> <T>*<F> | <T>/<F> | <F>", "<F> -> <F><G> | <G>",
                         "<G> -> 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9"};
        const string language = "15*9";
        for (const auto &i: g) {
            rule r(i);
            gmr.rules.emplace_back(r, false);
        }
        gmr.print();
        cout << "======================================\n";
        parser psr(language);
    } else {
        int n;
        cin >> n;
        cin.ignore();
        for (auto i = 0; i < n; i++) {
            rule r;
            gmr.rules.emplace_back(r, false);
        }
        string language;
        cin >> language;
        parser psr(language);
    }
}