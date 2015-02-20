using UnityEngine;
using System.Collections;
using System.Collections.Generic;

public class Game : MonoBehaviour {

	public GameObject prototype;
	public GameObject board;

	public int minBalls;
	public int maxBalls;

	private List<GameObject> balls;

	// Use this for initialization
	void Start () {
		this.balls = new List<GameObject> ();
		int count = Mathf.RoundToInt( Random.Range (minBalls, maxBalls));
		for (int i = 0; i < count; i++) {
			this.balls.Add (Instantiate (prototype, new Vector3 (0, 0, 0), Quaternion.identity) as GameObject);
		}

		// position ball somewhere on the board
	}


	// Update is called once per frame
	void Update () {
	
	}
}
