import * as React from "react";

type User = object;

export interface AuthContextType {
  user: User | null;
  signIn: (email: string, password: string) => Promise<User>;
  signUp: (email: string, password: string) => Promise<User>;
  signOut: () => Promise<void>;
  sendPasswordResetEmail: (email: string) => Promise<void>;
  confirmPasswordReset: (code: string, password: string) => Promise<void>;
}

const authContext = React.createContext<AuthContextType | undefined>(undefined);

export const ProvideAuth = ({ children }: { children: React.ReactChild | React.ReactChild[] }) =>
  <authContext.Provider value={useApiAuth()}>{children}</authContext.Provider>;

export const useAuth = () => {
  const auth = React.useContext(authContext);
  if (auth === undefined) {
    throw new Error('You can only use `useAuth` from inside an AuthProvider');
  }
  return auth;
}

function useMockAuth(): AuthContextType {
  const [user, setUser] = React.useState<User | null>(null);
  return {
    user,
    async signIn() {
      setUser({});
      return {};
    },
    async signUp() {
      setUser({});
      return {};
    },
    async signOut() {
      setUser(null);
    },
    async sendPasswordResetEmail(email: string) {
    },
    async confirmPasswordReset() {
    },
  };
}

function useApiAuth(): AuthContextType {
  const [user, setUser] = React.useState<User | null>(null);

  // Subscribe to user on mount
  // Because this sets state in the callback it will cause any ...
  // ... component that utilizes this hook to re-render with the ...
  // ... latest auth object.
  /* useEffect(() => {
   *   const unsubscribe = firebase.auth().onAuthStateChanged((user) => {
   *     if (user) {
   *       setUser(user);
   *     } else {
   *       setUser(null);
   *     }
   *   });
   *   return () => unsubscribe();
   * }, []); */

  return {
    user,
    async signIn(email: string, password: string) {
      // const response = await signInWithEmailAndPassword(email, password);
      const response = { user: {} };
      setUser(response.user);
      return response.user;
    },
    async signUp(email: string, password: string) {
      // const response = await createUserWithEmailAndPassword(email, password);
      const response = { user: {} };
      setUser(response.user);
      return response.user;
    },
    async signOut() {
      // await signOut()
      setUser(null);
    },
    async sendPasswordResetEmail(email: string) {
      // await sendPasswordResetEmail(email);
    },
    async confirmPasswordReset(code: string, password: string) {
      // await confirmPasswordReset(code, password)
    }
  };
}
