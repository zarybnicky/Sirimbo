import { synchronizeAthletes } from './index.ts'
import { Pool } from 'pg';

const client = new Pool();
synchronizeAthletes(client, {
  init: {
    referrer: 'https://www.csts.cz/dancesport/kalendar_akci',
  },
  onFetchError(idt, error) {
    console.error(idt, error);
  },
});
