import React from 'react';
import { ActivityIndicator, StyleSheet, Text, View } from 'react-native';
import { useQuery } from 'urql';
import { useAuth } from '../auth/AuthProvider';
import { Card } from '../components/Card';
import { Screen } from '../components/Screen';
import {
  TRAINER_SCHEDULE_QUERY,
  type TrainerScheduleResponse,
} from '../graphql/documents';

export function TrainerDashboard() {
  const { user } = useAuth();
  const trainerIds = React.useMemo(
    () =>
      user?.userProxiesList
        .map((proxy) => proxy.person)
        .filter((person): person is NonNullable<typeof person> => Boolean(person && (person.isTrainer || person.isAdmin)))
        .map((person) => person.id) ?? [],
    [user],
  );

  const scheduleVariables = React.useMemo(() => {
    const now = Date.now();
    return {
      start: new Date(now).toISOString(),
      end: new Date(now + 7 * 24 * 60 * 60 * 1000).toISOString(),
      trainerIds,
    };
  }, [trainerIds]);

  const [{ data, fetching, error }] = useQuery<TrainerScheduleResponse>({
    query: TRAINER_SCHEDULE_QUERY,
    variables: scheduleVariables,
    pause: trainerIds.length === 0,
  });

  if (!trainerIds.length) {
    return (
      <Screen>
        <Card>
          <Text style={styles.info}>
            K účtu není přiřazen žádný trenérský profil. Kontaktujte administrátora, pokud jde o chybu.
          </Text>
        </Card>
      </Screen>
    );
  }

  return (
    <Screen>
      <Card>
        <Text style={styles.heading}>Nadcházející lekce</Text>
        <Text style={styles.subtitle}>Zobrazuji týdenní přehled pro vaše trenérské profily.</Text>
      </Card>
      {fetching ? (
        <Card>
          <View style={styles.centeredRow}>
            <ActivityIndicator color="#2f80ed" />
            <Text style={styles.loadingText}>Načítám rozvrh…</Text>
          </View>
        </Card>
      ) : error ? (
        <Card>
          <Text style={styles.error}>Nepodařilo se načíst rozvrh: {error.message}</Text>
        </Card>
      ) : data?.list?.length ? (
        data.list.map((instance) => (
          <Card key={instance.id}>
            <Text style={styles.eventTitle}>{instance.event?.name ?? 'Událost'}</Text>
            <Text style={styles.eventMeta}>{formatRange(instance.since, instance.until)}</Text>
            {instance.event?.locationText ? (
              <Text style={styles.eventMeta}>Místo: {instance.event.locationText}</Text>
            ) : null}
            {instance.event?.summary ? (
              <Text style={styles.eventSummary}>{instance.event.summary}</Text>
            ) : null}
            {instance.trainers?.length ? (
              <Text style={styles.eventMeta}>
                Trenéři: {instance.trainers.map((trainer) => trainer.name).join(', ')}
              </Text>
            ) : null}
            <Text style={[styles.status, instance.isCancelled ? styles.statusCancelled : undefined]}>
              {instance.isCancelled ? 'Zrušeno' : 'Aktivní'}
            </Text>
          </Card>
        ))
      ) : (
        <Card>
          <Text style={styles.info}>V následujícím týdnu nemáte žádné naplánované lekce.</Text>
        </Card>
      )}
    </Screen>
  );
}

function formatRange(startIso: string, endIso: string) {
  const start = new Date(startIso);
  const end = new Date(endIso);
  const formatter = new Intl.DateTimeFormat('cs-CZ', {
    weekday: 'short',
    day: '2-digit',
    month: '2-digit',
    hour: '2-digit',
    minute: '2-digit',
  });
  return `${formatter.format(start)} – ${new Intl.DateTimeFormat('cs-CZ', {
    hour: '2-digit',
    minute: '2-digit',
  }).format(end)}`;
}

const styles = StyleSheet.create({
  heading: {
    fontSize: 24,
    fontWeight: '700',
  },
  subtitle: {
    fontSize: 16,
    color: '#4f4f4f',
  },
  info: {
    fontSize: 16,
    color: '#4f4f4f',
  },
  centeredRow: {
    flexDirection: 'row',
    alignItems: 'center',
  },
  loadingText: {
    fontSize: 16,
    color: '#4f4f4f',
    marginLeft: 12,
  },
  error: {
    color: '#d32f2f',
    fontSize: 16,
  },
  eventTitle: {
    fontSize: 18,
    fontWeight: '700',
  },
  eventMeta: {
    fontSize: 14,
    color: '#4f4f4f',
  },
  eventSummary: {
    marginTop: 8,
    color: '#333333',
  },
  status: {
    marginTop: 8,
    fontWeight: '600',
    color: '#2f80ed',
  },
  statusCancelled: {
    color: '#d32f2f',
  },
});
