import './main.css';
import { Fifa } from './Fifa.elm';
import registerServiceWorker from './registerServiceWorker';

Fifa.embed(document.getElementById('root'));

registerServiceWorker();
