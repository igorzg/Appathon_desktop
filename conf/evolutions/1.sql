# Users schema

# --- !Ups

CREATE TABLE "Codes" (
    "code_id" int(10) NOT NULL AUTO_INCREMENT,
    "code" VARCHAR (255) NOT NULL,
    "cash" DOUBLE (255) UNIQUE NOT NULL,
    PRIMARY KEY ("user_id")
);

# --- !Downs

DROP TABLE "User";