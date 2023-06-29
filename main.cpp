#include <iostream>
#include <utility>
#include <vector>
#include <algorithm>
#include <string>
#include <sstream>
#include <bitset>
#include <cmath>
#include <unordered_set>

using namespace std;

class rule {
public:
    string hs;
    string lhs;
    vector<string> rhs;

    // parse string to lhs and rhs
    void parseHs() {
        hs.erase(std::remove(hs.begin(), hs.end(), ' '), hs.end());
        stringstream ss(hs);
        string token;
        vector<string> temp;
        while (getline(ss, token, '-'))
            temp.push_back(token);
        lhs = temp[0];
        stringstream sss(temp[1].erase(0, 1));
        while (getline(sss, token, '|'))
            rhs.push_back(token);
        vector<string>().swap(temp);
    }

    void print() {
        cout << lhs << ":  ";
        for (const auto &i: rhs)
            cout << i << " | ";
        //cout << endl;
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

class grammar {
public:
    vector<pair<rule, bool>> rules;
    vector<string> nullVar;
    vector<string> newProduction;
    vector<vector<int>> unitProPos;

    void print() {
        for (auto i: rules) {
            i.first.print();
            cout << "  (" << i.second << ")" << endl;
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
                            if (up.empty()) up = "*";
                            newProduction.push_back(up);
                        }
                    }
                    // add new production to rhs of rule
                    gram.first.rhs.insert(gram.first.rhs.end(), newProduction.begin(), newProduction.end());
                    // remove duplicate from rhs
                    unordered_set<string> rmdp;
                    auto end = std::remove_copy_if(gram.first.rhs.begin(), gram.first.rhs.end(), gram.first.rhs.begin(),
                                                   [&rmdp](string const &i) {
                                                       return !rmdp.insert(i).second;
                                                   });
                    gram.first.rhs.erase(end, gram.first.rhs.end());
                }
            }
            int ind{};
            for (auto &i: rules) {
                for (auto &j: i.first.rhs) {
                    // set flag of rule that their null production is removed
                    if (j == "#") {
                        i.first.rhs.erase(remove(i.first.rhs.begin(), i.first.rhs.end(), "#"), i.first.rhs.end());
                        i.second = true;
                        break;
                    } else if (j == "*") {
                        j = "#";
                    }
                }
                // remove rule with empty rhs
                if (i.first.rhs.empty())
                    rules.erase(rules.begin() + ind, rules.begin() + ind + 1);
                ind++;
            }
            print();
            cout << "======================================\n";
        }
    }

    // search the grammar for nullable variable and if find one return true else return false
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
                cout << i[0] << " " << i[1] << " " << i[2] << endl;
                // remove unit production
                rules[i[0]].first.rhs.erase(rules[i[0]].first.rhs.begin()+i[1]);
                // replace unit production with its rules
                rules[i[0]].first.rhs.insert(rules[i[0]].first.rhs.end(), rules[i[2]].first.rhs.begin(), rules[i[2]].first.rhs.end());
                // remove duplicate production
                unordered_set<string> rmdp;
                auto end = std::remove_copy_if(rules[i[0]].first.rhs.begin(), rules[i[0]].first.rhs.end(), rules[i[0]].first.rhs.begin(),
                                               [&rmdp](string const &i) {
                                                   return !rmdp.insert(i).second;
                                               });
                rules[i[0]].first.rhs.erase(end, rules[i[0]].first.rhs.end());
            }
            print();
            cout << "======================================\n";
        }
    }

    // search the grammar for unit production and if find one return true else return false
    bool findUnitPro() {
        unitProPos.clear();
        unitProPos.shrink_to_fit();
        int x{}, y{}, z{};
        vector<int> position;
        for (const auto &i: rules) {
            for (const auto &j: rules) {
                for (const auto &k: i.first.rhs) {
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

} gmr;

int main() {
    /* int n;
    cin >> n;
    cin.ignore();
    for (auto i = 0; i < n; i++) {
        rule r;
        gmr.rules.emplace_back(r);
    }
    string language;
    cin >> language;
    */
    const string g[]{"<S> -> a<S>b | a<A> | b<B>", "<A> -> a<A> | <A>a<A>a | #", "<B> -> b<B> | #", "<C> -> <A>"};
    const string language = "aaab";
    for (const auto &i: g) {
        rule r(i);
        gmr.rules.emplace_back(r, false);
    }
    gmr.print();
    cout << "======================================\n";
    gmr.rmNullVar();
    gmr.rmUnitPro();
}

// 3
// <S> -> a<S>b | a<A> | b<B>
// <A> -> a<A> | #
// <B> -> b<B> | #
// aaab