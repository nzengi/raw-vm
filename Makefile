.PHONY: clean compile run run-node1 run-node2 test package docker

# Default hedef
all: compile

# Temizleme
clean:
	sbt clean

# Derleme
compile:
	sbt compile

# Test etme
test:
	sbt test

# Paketi oluşturma
package:
	sbt assembly

# Dockerize etme
docker:
	sbt docker:publishLocal

# Ana düğümü çalıştırma
run-node1:
	sbt "run --node-id node-1 --endpoint 127.0.0.1:9000"

# İkinci düğümü çalıştırma (bootstrap'lı)
run-node2:
	sbt "run --node-id node-2 --endpoint 127.0.0.1:9001 --bootstrap 127.0.0.1:9000"

# Genel çalıştırma
run:
	sbt run

# Çoklu düğüm ağı başlatma
network-up:
	@echo "3 düğümlü test ağı başlatılıyor..."
	@mkdir -p logs
	@sbt "run --node-id node-1 --endpoint 127.0.0.1:9000" > logs/node1.log 2>&1 & echo $$! > .node1.pid
	@sleep 5
	@sbt "run --node-id node-2 --endpoint 127.0.0.1:9001 --bootstrap 127.0.0.1:9000" > logs/node2.log 2>&1 & echo $$! > .node2.pid
	@sleep 2
	@sbt "run --node-id node-3 --endpoint 127.0.0.1:9002 --bootstrap 127.0.0.1:9000,127.0.0.1:9001" > logs/node3.log 2>&1 & echo $$! > .node3.pid
	@echo "Test ağı başlatıldı. Log dosyaları 'logs/' dizininde."

# Çoklu düğüm ağını durdurma
network-down:
	@echo "Test ağı durduruluyor..."
	@-kill `cat .node1.pid` 2>/dev/null || true
	@-kill `cat .node2.pid` 2>/dev/null || true
	@-kill `cat .node3.pid` 2>/dev/null || true
	@rm -f .node*.pid
	@echo "Test ağı durduruldu."

# Yardım
help:
	@echo "Kullanılabilir hedefler:"
	@echo "  all        : Projeyi derle (varsayılan)"
	@echo "  clean      : Derleme artefaktlarını temizle"
	@echo "  compile    : Projeyi derle"
	@echo "  test       : Testleri çalıştır"
	@echo "  package    : Çalıştırılabilir JAR oluştur"
	@echo "  docker     : Docker imajı oluştur"
	@echo "  run        : Varsayılan düğümü çalıştır"
	@echo "  run-node1  : İlk düğümü çalıştır (bootstrap)"
	@echo "  run-node2  : İkinci düğümü çalıştır (ilkine bağlanan)"
	@echo "  network-up : 3 düğümlü test ağını başlat"
	@echo "  network-down : Test ağını durdur" 