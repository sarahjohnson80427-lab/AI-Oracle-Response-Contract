# 🤖 AI Oracle Response Contract

A decentralized AI oracle contract where users ask smart contract questions, off-chain AI agents compute answers, cryptographically prove authenticity, and receive automatic on-chain payments.

This MVP focuses on oracle trust models, incentive design, and secure off-chain computation.

---

## 🌟 Key Capabilities

- 🧠 On-chain question registry
- 🔐 Signature-based AI verification
- ⏳ Expiring oracle requests
- 💸 Automatic STX payouts
- 🔁 Refunds for unanswered questions
- 📊 Oracle reputation tracking

---

## 🏗 Architecture

- Users pay a fixed oracle fee per question
- Off-chain AI signs answer hashes
- Contract verifies signatures using secp256k1
- Funds are released only on valid responses
- Questions expire and become refundable

---

## ⚙️ Usage

### Deploy
```bash
clarinet deploy
