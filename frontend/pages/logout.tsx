import React from "react";
import { useAuth } from "lib/data/use-auth";
import { useRouter } from "next/router";

export default function LogoutPage() {
  const { signOut } = useAuth();
  const router = useRouter();
  React.useEffect(() => {
    signOut();
    router.push('/');
  }, []);
  return null;
};
