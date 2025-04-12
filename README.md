# ZK-DAG-PoS Blockchain Projesi

Bu proje, nesne tabanlı, DAG (Yönlendirilmiş Asiklik Graf) yapısı üzerine kurulu, Proof of Stake konsensus mekanizması kullanan ve Zero-Knowledge kanıtları destekleyen yenilikçi bir blockchain platformudur.

## Özellikler

- **DAG Tabanlı**: Paralel işleme ile yüksek verimlilik
- **Nesne Merkezli**: Sui ve Move'dan ilham alan nesne merkezli programlama modeli
- **Proof of Stake**: Enerji verimli konsensus mekanizması
- **Zero-Knowledge Entegrasyonu**: Özel işlemler ve doğrulanabilir hesaplamalar
- **Ölçeklenebilir Mimari**: Yüksek performans için tasarlanmış

## Teknoloji Yığını

- **Dil**: Scala 2.13
- **Ağ İletişimi**: Akka Actor, Akka HTTP
- **Kriptografi**: Bouncy Castle
- **Veri İşleme**: Circe (JSON)

## Kullanılan Algoritmalar

- **Konsensus**: BullShark / HotStuff varyantı
- **Kriptografi**: Ed25519, BLS imzaları
- **Zero-Knowledge**: PlonK, zk-SNARKs
- **Eşzamanlılık Kontrolü**: MVCC (Multi-Version Concurrency Control)

## Proje Yapısı

Proje ana bileşenleri:

- **core/**: Ana modüller ve alt sistemler

  - `DAGConsensus.scala`: DAG tabanlı konsensus mekanizması
  - `ObjectModel.scala`: Nesne modeli ve depolama
  - `ZKSystem.scala`: Zero-Knowledge sistem entegrasyonu
  - `PoSStaking.scala`: Proof of Stake ve validatör yönetimi
  - `NetworkNode.scala`: P2P ağ iletişimi
  - `Blockchain.scala`: Ana blockchain mantığı
  - `Main.scala`: Uygulama giriş noktası

- **provi/**: VM ve yürütme modüleri
  - `sca_executor.scala`: VM operasyon yürütücüsü

## Başlangıç

### Gereksinimler

- JDK 11+
- SBT (Scala Build Tool)

### Kurulum

```bash
# Projeyi klonla
git clone https://github.com/username/zk-dag-pos-blockchain.git
cd zk-dag-pos-blockchain

# Derle
sbt compile

# Çalıştır (Genesis düğümü)
sbt "run --node-id node-1 --endpoint 127.0.0.1:9000"

# İkinci düğüm (başka bir terminalde)
sbt "run --node-id node-2 --endpoint 127.0.0.1:9001 --bootstrap 127.0.0.1:9000"
```

### Komut Satırı Arayüzü

Düğüm başlatıldığında aşağıdaki komutları kullanabilirsiniz:

- `info` - Blockchain bilgisini gösterir
- `validator register <isim> <miktar> <oran>` - Doğrulayıcı olarak kaydolur
- `stake <validator-id> <miktar>` - Bir doğrulayıcıya stake ekler
- `object create <tip> <veri>` - Yeni bir nesne oluşturur
- `help` - Tüm komutları listeler
- `exit` - Düğümü kapatır

## Mimari

Bu blockchain platformu aşağıdaki ana bileşenlerden oluşur:

1. **Nesne Deposu**: Blockchain'in dünya durumunu nesne olarak depolar
2. **DAG Konsensus**: İşlemleri paralel olarak işleyip sıralayan sistem
3. **Staking Sistemi**: Doğrulayıcıları ve stake'leri yöneten PoS mekanizması
4. **ZK Sistemi**: Zero-Knowledge kanıtlarını doğrulayan ve yöneten modül
5. **Ağ İletişimi**: P2P protokolünü yöneten bileşen

## Geliştirme Planı

- [ ] Temel konsensus ve nesne modelinin tamamlanması
- [ ] Ağ iletişiminin iyileştirilmesi (Akka HTTP/WebSocket)
- [ ] ZK entegrasyonunun gerçek kriptografi kütüphaneleriyle geliştirilmesi
- [ ] Smart Contract desteği (WASM veya özel VM)
- [ ] Kapsamlı test ve performans iyileştirmeleri

## Katkıda Bulunma

Katkıda bulunmak için:

1. Bu projeyi fork'layın
2. Değişikliklerinizi yapın ve test edin
3. Pull request gönderin

## Lisans

Bu proje MIT lisansı altında lisanslanmıştır. Detaylar için [LICENSE](LICENSE) dosyasına bakın.
