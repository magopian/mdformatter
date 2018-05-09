import './main.css';
import { Main } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

const storedContent = localStorage.getItem("storedContent") || "[]";
const app = Main.embed(document.getElementById('root'), JSON.parse(storedContent));

app.ports.storeContent.subscribe(function(content) {
  localStorage.setItem("storedContent", JSON.stringify(content));
});

registerServiceWorker();
