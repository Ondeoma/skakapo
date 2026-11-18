# skakapo アプリ（scripts/dist.sh の成果物を詰めるだけ）
FROM eclipse-temurin:21-jre-alpine
# エージェントがリポジトリを取り出して push するのに git を使う
RUN apk add --no-cache git && addgroup -S skakapo && adduser -S -G skakapo skakapo && mkdir /work && chown skakapo:skakapo /work
COPY dist/lib /app/lib
COPY dist/static /app/static
ENV STATIC_DIR=/app/static HTTP_PORT=8080 WORK_DIR=/work
USER skakapo
EXPOSE 8080
CMD ["java", "-XX:MaxRAMPercentage=70", "-cp", "/app/lib/*", "skakapo.app.Main"]
