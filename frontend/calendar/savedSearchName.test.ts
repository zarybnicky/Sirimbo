import assert from 'node:assert/strict';
import { test } from 'node:test';
import {
  automaticSearchNamePrefix,
  savedSearchName,
  suggestSavedSearchName,
} from './savedSearchName.ts';

test('default and restricted types produce readable titles', () => {
  assert.equal(suggestSavedSearchName({ condition: { onlyMine: true } }), 'Moje události');
  assert.equal(suggestSavedSearchName({ condition: { types: ['LESSON'] } }), 'Lekce');
  assert.equal(suggestSavedSearchName({
    condition: { types: [], withCompetitions: true, withBirthdays: true },
  }), 'Soutěže, Narozeniny');
  assert.equal(suggestSavedSearchName({
    condition: { canRegister: true, onlyMine: true },
  }), 'Moje volné lekce');
});

test('the suggestion uses selected names and distinguishes teaching-only rows', () => {
  assert.equal(suggestSavedSearchName({
    condition: {
      types: ['LESSON'], participantIds: ['1'], trainerIds: ['2'],
      coupleId: '3', cohortId: '4', locationIds: ['5'],
    },
  }, {
    people: { nodes: [
      { id: '1', name: 'Jan Novák' }, { id: '2', name: 'Eva Malá' },
      { id: '9', name: 'Nevybraný člověk' },
    ] },
    couplesList: [{ id: '3', name: 'Petr a Anna' }],
    tenant: {
      cohortsList: [{ id: '4', name: 'TS1' }],
      locationsList: [{ id: '5', name: 'Velký sál' }],
    },
  }), 'Lekce – Jan Novák – Eva Malá (výuka) – Petr a Anna – TS1 – Velký sál');
});

test('the stored automatic name has the same visible label in the app and export', () => {
  const label = suggestSavedSearchName({ condition: { types: ['LESSON'] } });
  const stored = automaticSearchNamePrefix + label;
  assert.ok(stored.startsWith(automaticSearchNamePrefix));
  assert.equal(savedSearchName(stored), label);
  assert.equal(`Klub – ${savedSearchName(stored)}`, 'Klub – Lekce');
  assert.equal(savedSearchName('Vlastní název'), 'Vlastní název');
});
