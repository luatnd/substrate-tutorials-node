// build both the native Rust binary (std) and the WebAssembly (no_std) binary.
// NOTE: All of the pallets used in a runtime must be set to compile with the no_std features.
#![cfg_attr(not(feature = "std"), no_std)]

// Re-export pallet items so that they can be accessed from the crate namespace.
pub use pallet::*;

#[frame_support::pallet]
pub mod pallet {
	use frame_support::{
		pallet_prelude::*,
		sp_runtime::traits::Hash, // support T::Hashing
		traits::{
			Randomness,
		},
	};
	use frame_system::pallet_prelude::*;
	use sp_std::vec::Vec; // Step 3.1 will include this in `Cargo.toml`
	// use sp_io::hashing::blake2_128;
	use scale_info::TypeInfo;

	// #[cfg(feature = "std")]
	// use frame_support::serde::{Deserialize, Serialize};

	/// Configure the pallet by specifying the parameters and types on which it depends.
	#[pallet::config]
	pub trait Config: frame_system::Config {
		/// Because this pallet emits events, it depends on the runtime's definition of an event.
		type Event: From<Event<Self>> + IsType<<Self as frame_system::Config>::Event>;
		type PoeRandomness: Randomness<Self::Hash, Self::BlockNumber>;
	}


	type ProofId<T> = <T as frame_system::Config>::Hash;
	type ProofPhoneValueType = Vec<u8>;
	type ProofFileValueType = Vec<u8>;

	// Pallets use events to inform users when important changes are made.
	// Event documentation should end with an array that provides descriptive names for parameters.
	// https://docs.substrate.io/v3/runtime/events-and-errors
	#[pallet::event]
	#[pallet::generate_deposit(pub(super) fn deposit_event)]  // TODO: what is generate_deposit?
	pub enum Event<T: Config> {
		/// Event emitted when a proof has been claimed. [who, claim]
		ClaimCreated(T::AccountId, ProofId<T>),
		/// Event emitted when a claim is revoked by the owner. [who, claim]
		ClaimPhoneRevoked(T::AccountId, ProofPhoneValueType),
		ClaimFileRevoked(T::AccountId, ProofFileValueType),
	}



	#[pallet::error]
	pub enum Error<T> {
		/// The proof has already been claimed.
		// ProofAlreadyClaimed,
		ProofPhoneAlreadyClaimed,
		ProofFileAlreadyClaimed,
		/// The proof does not exist, so it cannot be revoked.
		NoSuchProof,
		/// The proof is claimed by another account, so caller can't revoke it.
		NotProofOwner,
		CannotUpdateProofProp,
	}



	#[pallet::pallet]
	#[pallet::generate_store(pub(super) trait Store)]
	// #[pallet::generate_storage_info]
	pub struct Pallet<T>(_);



	/// Struct for holding Order information.
	#[derive(Clone, Encode, Decode, PartialEq, RuntimeDebug, TypeInfo)]
	#[scale_info(skip_type_params(T))]
	pub struct Proof<T: Config> {
		pub id: ProofId<T>,
		pub account_id: T::AccountId,
		pub block_number: T::BlockNumber,
		pub phone: Option<ProofPhoneValueType>,
		pub file: Option<ProofFileValueType>,
	}



	/*
	This Proof<T> must use OptionalQuery instead of ValueQuery
	144 |             <UserProofs<T>>::insert(sender.clone(), proof);
    |                              ^^^^^^ function or associated item cannot be called on `frame_support::pallet_prelude::StorageMap<_GeneratedPrefixForStorageUserProofs<T>, frame_support::Blake2_128Concat, <T as frame_system::Config>::AccountId, Proof<T>, frame_support::pallet_prelude::ValueQuery>` due to unsatisfied trait bounds
	 */
	#[pallet::storage]
	pub(super) type UserProofs<T: Config> = StorageMap<_, Blake2_128Concat, T::AccountId, Proof<T>>;

	#[pallet::storage]
	pub(super) type ProofPhones<T: Config> = StorageMap<_, Blake2_128Concat, ProofPhoneValueType, T::AccountId, ValueQuery>;

	#[pallet::storage]
	pub(super) type ProofFiles<T: Config> = StorageMap<_, Blake2_128Concat, ProofFileValueType, T::AccountId, ValueQuery>;



	#[pallet::hooks]
	impl<T: Config> Hooks<BlockNumberFor<T>> for Pallet<T> {}


	// Dispatchable functions allow users to interact with the pallet and invoke state changes.
	// These functions materialize as "extrinsics", which are often compared to transactions.
	// Dispatchable functions must be annotated with a weight and must return a DispatchResult.
	#[pallet::call]
	impl<T: Config> Pallet<T> {
		#[pallet::weight(1_000)]
		pub fn create_claim(
			origin: OriginFor<T>,
			phone: Option<ProofPhoneValueType>,
			file: Option<ProofFileValueType>,
		) -> DispatchResult {
			// Check that the extrinsic was signed and get the signer.
			// This function will return an error if the extrinsic is not signed.
			// https://docs.substrate.io/v3/runtime/origins
			let sender = ensure_signed(origin)?;

			// Verify that the specified proof has not already been claimed.
			if phone.is_some() {
				ensure!(!ProofPhones::<T>::contains_key(phone.as_ref().unwrap()), Error::<T>::ProofPhoneAlreadyClaimed);
			}
			if file.is_some() {
				ensure!(!ProofFiles::<T>::contains_key(file.as_ref().unwrap()), Error::<T>::ProofFileAlreadyClaimed);
			}


			// Get the block number from the FRAME System pallet.
			let current_block = <frame_system::Pallet<T>>::block_number();

			// Store the proof with the sender and block number.
			// create orders
			let id = Self::gen_unique_id();
			if UserProofs::<T>::contains_key(sender.clone()) {
				// update
				if phone.is_some() {
					// Self::update_user_proof_phone(&sender, phone.clone())
					// 	.map_err(|_:T| <Error<T>>::CannotUpdateProofProp)? // TODO: Check if it not panic because of type T
					// ;
					<UserProofs<T>>::try_mutate(&sender, |proof| {
						if proof.is_some() {
							// TODO: Is this really changed?
							// TODO: Unit test
							proof.as_mut().unwrap().phone = phone.clone();
						}
						Ok(())
					}).map_err(|_:T| <Error<T>>::CannotUpdateProofProp)?; // TODO: Check if it not panic because of type T

				}
				if file.is_some() {
					// Self::update_user_proof_file(&sender, &file as Option<ProofFileValueType>)
					// 	.map_err(|_:T| <Error<T>>::CannotUpdateProofProp)? // TODO: Check if it not panic because of type T
					// ;
					<UserProofs<T>>::try_mutate(&sender, |proof| {
						if proof.is_some() {
							// TODO: Is this really changed?
							// TODO: Unit test
							proof.as_mut().unwrap().file = file.clone();
						}
						Ok(())
					}).map_err(|_:T| <Error<T>>::CannotUpdateProofProp)?;
				}
			} else {
				// create
				<UserProofs<T>>::insert(sender.clone(), Proof::<T> {
					id,
					account_id: sender.clone(),
					block_number: current_block,
					phone: phone.clone(),
					file: file.clone(),
				});
			}

			if phone.is_some() {
				ProofPhones::<T>::insert(phone.unwrap(), sender.clone());
			}
			if file.is_some() {
				ProofFiles::<T>::insert(file.unwrap(), sender.clone());
			}

			// Emit an event that the claim was created.
			Self::deposit_event(Event::ClaimCreated(sender, id));

			Ok(())
		}

		#[pallet::weight(10_000)]
		pub fn revoke_claim_phone(
			origin: OriginFor<T>,
			phone: ProofPhoneValueType,
		) -> DispatchResult {
			// Check that the extrinsic was signed and get the signer.
			// This function will return an error if the extrinsic is not signed.
			// https://docs.substrate.io/v3/runtime/origins
			let sender = ensure_signed(origin)?;

			// Verify that the specified proof has been claimed.
			ensure!(ProofPhones::<T>::contains_key(&phone), Error::<T>::NoSuchProof);

			// Get owner of the claim.
			let owner = ProofPhones::<T>::get(&phone);

			// Verify that sender of the current call is the claim owner.
			ensure!(sender == owner, Error::<T>::NotProofOwner);

			// Remove claim from storage.
			// Performs this operation first because as it may fail
			// Self::update_user_proof_phone(&sender, &phone as Option<ProofPhoneValueType>)
			// 	.map_err(|_:T| <Error<T>>::CannotUpdateProofProp)? // TODO: Check if it not panic because of type T
			// ;
			<UserProofs<T>>::try_mutate(&sender, |proof| {
				if proof.is_some() {
					// TODO: Is this really changed?
					// TODO: Unit test
					proof.as_mut().unwrap().phone = Some(phone.clone());
				}
				Ok(())
			}).map_err(|_:T| <Error<T>>::CannotUpdateProofProp)?; // TODO: Check if it not panic because of type T
			ProofPhones::<T>::remove(&phone);

			// Emit an event that the claim was erased.
			Self::deposit_event(Event::ClaimPhoneRevoked(sender, phone));
			Ok(())
		}

		#[pallet::weight(10_000)]
		pub fn revoke_claim_file(
			origin: OriginFor<T>,
			file: ProofFileValueType,
		) -> DispatchResult {
			// Check that the extrinsic was signed and get the signer.
			// This function will return an error if the extrinsic is not signed.
			// https://docs.substrate.io/v3/runtime/origins
			let sender = ensure_signed(origin)?;

			// Verify that the specified proof has been claimed.
			ensure!(ProofPhones::<T>::contains_key(&file), Error::<T>::NoSuchProof);

			// Get owner of the claim.
			let owner = ProofPhones::<T>::get(&file);

			// Verify that sender of the current call is the claim owner.
			ensure!(sender == owner, Error::<T>::NotProofOwner);

			// Remove claim from storage.
			// Performs this operation first because as it may fail
			// Self::update_user_proof_phone(&sender, &phone as Option<ProofPhoneValueType>)
			// 	.map_err(|_:T| <Error<T>>::CannotUpdateProofProp)? // TODO: Check if it not panic because of type T
			// ;
			<UserProofs<T>>::try_mutate(&sender, |proof| {
				if proof.is_some() {
					// TODO: Is this really changed?
					// TODO: Unit test
					proof.as_mut().unwrap().file = Some(file.clone());
				}
				Ok(())
			}).map_err(|_:T| <Error<T>>::CannotUpdateProofProp)?; // TODO: Check if it not panic because of type T
			ProofFiles::<T>::remove(&file);

			// Emit an event that the claim was erased.
			Self::deposit_event(Event::ClaimFileRevoked(sender, file));
			Ok(())
		}
	}


	impl<T: Config> Pallet<T> {
		fn gen_unique_id() -> T::Hash {
			// let block_number = <frame_system::Pallet<T>>::block_number();
			let rnd_str = T::PoeRandomness::random(&b"my_seed"[..]).0;
			T::Hashing::hash_of(&rnd_str)
		}

		// fn update_user_proof_phone(sender: T::AccountId, value: Option<ProofPhoneValueType>) -> Result<(), _> {
		// 	<UserProofs<T>>::try_mutate(sender, |proof| {
		// 		if proof.is_some() {
		// 			// TODO: Is this really changed?
		// 			// TODO: Unit test
		// 			proof.as_mut().unwrap().phone = value;
		// 		}
		// 		Ok(())
		// 	})
		// }
		//
		// fn update_user_proof_file(sender: T::AccountId, value: Option<ProofPhoneValueType>) -> Result<(), _> {
		// 	<UserProofs<T>>::try_mutate(sender, |proof| {
		// 		if proof.is_some() {
		// 			// TODO: Is this really changed?
		// 			// TODO: Unit test
		// 			proof.as_mut().unwrap().file = value;
		// 		}
		// 		Ok(())
		// 	})
		// }
	}
}
