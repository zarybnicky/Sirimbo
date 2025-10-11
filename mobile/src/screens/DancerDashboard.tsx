import React from 'react';
import { ActivityIndicator, StyleSheet, Text, View } from 'react-native';
import { useQuery } from 'urql';
import { useAuth } from '../auth/AuthProvider';
import { Card } from '../components/Card';
import { Screen } from '../components/Screen';
import {
  PERSON_ATTENDANCE_QUERY,
  type PersonAttendanceResponse,
} from '../graphql/documents';

export function DancerDashboard() {
  const { user } = useAuth();
  const activeProxy = user?.userProxiesList.find((proxy) => proxy.person);
  const person = activeProxy?.person ?? null;

  const variables = React.useMemo(() => (person ? { id: person.id } : undefined), [person]);
  const [{ data, fetching, error }] = useQuery<PersonAttendanceResponse>({
    query: PERSON_ATTENDANCE_QUERY,
    variables,
    pause: !person,
  });

  return (
    <Screen>
      <Card>
        <Text style={styles.heading}>Vítejte</Text>
        <Text style={styles.subtitle}>
          {person ? `Přihlášen jako ${person.firstName ?? ''} ${person.lastName ?? ''}`.trim() :
            'K vašemu účtu zatím není přiřazena osoba.'}
        </Text>
      </Card>
      {!person ? (
        <Card>
          <Text style={styles.info}>
            Požádejte administrátora o propojení osoby s účtem, abyste viděli rozvrh lekcí.
          </Text>
        </Card>
      ) : fetching ? (
        <Card>
          <View style={styles.centeredRow}>
            <ActivityIndicator color="#2f80ed" />
            <Text style={styles.loadingText}>Načítám poslední lekce…</Text>
          </View>
        </Card>
      ) : error ? (
        <Card>
          <Text style={styles.error}>Nepodařilo se načíst docházku: {error.message}</Text>
        </Card>
      ) : data?.person?.recentAttendanceList?.length ? (
        data.person.recentAttendanceList.map((attendance) => {
          if (!attendance.instance) return null;
          const { instance } = attendance;
          const summary = instance.event?.summary ?? instance.event?.name ?? 'Událost';
          return (
            <Card key={attendance.id}>
              <Text style={styles.eventTitle}>{summary}</Text>
              <Text style={styles.eventMeta}>{formatRange(instance.since, instance.until)}</Text>
              {instance.event?.locationText ? (
                <Text style={styles.eventMeta}>Místo: {instance.event.locationText}</Text>
              ) : null}
              {instance.trainers?.length ? (
                <Text style={styles.eventMeta}>
                  Trenéři: {instance.trainers.map((trainer) => trainer.name).join(', ')}
                </Text>
              ) : null}
              <Text style={styles.attendanceStatus}>Stav: {translateStatus(attendance.status)}</Text>
            </Card>
          );
        })
      ) : (
        <Card>
          <Text style={styles.info}>Zatím tu nejsou žádné záznamy o účasti.</Text>
        </Card>
      )}
    </Screen>
  );
}

function translateStatus(status: string) {
  switch (status) {
    case 'ATTENDED':
      return 'Navštíveno';
    case 'REGISTERED':
      return 'Přihlášen';
    case 'CANCELLED':
      return 'Zrušeno';
    default:
      return status.toLowerCase();
  }
}

function formatRange(startIso: string, endIso: string) {
  const start = new Date(startIso);
  const end = new Date(endIso);
  return `${start.toLocaleDateString('cs-CZ', {
    weekday: 'short',
    day: '2-digit',
    month: '2-digit',
  })} ${start.toLocaleTimeString('cs-CZ', {
    hour: '2-digit',
    minute: '2-digit',
  })} – ${end.toLocaleTimeString('cs-CZ', {
    hour: '2-digit',
    minute: '2-digit',
  })}`;
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
  error: {
    color: '#d32f2f',
    fontSize: 16,
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
  eventTitle: {
    fontSize: 18,
    fontWeight: '700',
  },
  eventMeta: {
    fontSize: 14,
    color: '#4f4f4f',
  },
  attendanceStatus: {
    marginTop: 8,
    fontWeight: '600',
  },
});
