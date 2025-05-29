FROM node:20

WORKDIR /app

COPY package*.json ./
RUN npm install

# COPY . .

# RUN npm run build

# Use preview server for production preview
# RUN npm install -g serve

EXPOSE 5173

CMD ["npm", "run", "dev"]

