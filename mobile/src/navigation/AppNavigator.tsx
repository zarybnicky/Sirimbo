import { createBottomTabNavigator } from '@react-navigation/bottom-tabs';
import { createNativeStackNavigator } from '@react-navigation/native-stack';
import { MaterialIcons } from '@expo/vector-icons';
import React from 'react';
import { useAuth } from '../auth/AuthProvider';
import { DancerDashboard } from '../screens/DancerDashboard';
import { LoadingScreen } from '../screens/LoadingScreen';
import { LoginScreen } from '../screens/LoginScreen';
import { TrainerDashboard } from '../screens/TrainerDashboard';
import { SettingsScreen } from '../screens/SettingsScreen';

const Stack = createNativeStackNavigator();
const Tabs = createBottomTabNavigator();

function MainTabs() {
  const { user } = useAuth();
  const canSeeTrainer = user?.userProxiesList.some(
    (proxy) => proxy.person?.isTrainer || proxy.person?.isAdmin,
  );

  return (
    <Tabs.Navigator
      screenOptions={({ route }) => ({
        headerShown: false,
        tabBarActiveTintColor: '#2f80ed',
        tabBarInactiveTintColor: '#828282',
        tabBarStyle: { paddingBottom: 6, height: 60 },
        tabBarIcon: ({ color, size }) => {
          switch (route.name) {
            case 'Dancer':
              return <MaterialIcons name="self-improvement" size={size} color={color} />;
            case 'Trainer':
              return <MaterialIcons name="emoji-people" size={size} color={color} />;
            default:
              return <MaterialIcons name="settings" size={size} color={color} />;
          }
        },
      })}
    >
      <Tabs.Screen name="Dancer" component={DancerDashboard} options={{ title: 'Tanečník' }} />
      {canSeeTrainer ? (
        <Tabs.Screen name="Trainer" component={TrainerDashboard} options={{ title: 'Trenér' }} />
      ) : null}
      <Tabs.Screen name="Settings" component={SettingsScreen} options={{ title: 'Nastavení' }} />
    </Tabs.Navigator>
  );
}

export function AppNavigator() {
  const { status } = useAuth();

  return (
    <Stack.Navigator screenOptions={{ headerShown: false }}>
      {status === 'authenticated' ? (
        <Stack.Screen name="Main" component={MainTabs} />
      ) : status === 'loading' || status === 'initializing' ? (
        <Stack.Screen name="Loading" component={LoadingScreen} />
      ) : (
        <Stack.Screen name="Login" component={LoginScreen} />
      )}
    </Stack.Navigator>
  );
}
