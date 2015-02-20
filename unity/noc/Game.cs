using UnityEngine;
using System.Collections;
using System.Collections.Generic;

[System.Serializable]
public class Bounds {
	public float minX;
	public float maxX;
	public float minZ;
	public float maxZ;
}
public class Game : MonoBehaviour {

	public GameObject prototype;
	public GameObject board;

	public int minBalls;
	public int maxBalls;
	public float minSize;
	public float maxSize;
	public Bounds bounds;

	private List<Sphere> balls;
	private List<Vector3> forces;

	// Use this for initialization
	void Start () {
		forces = new List<Vector3> ();
		forces.Add (new Vector3 (0.5f, 0f, 0f));
		forces.Add (new Vector3 (0f, 0f, -0.1f));

		this.balls = new List<Sphere> ();
		int count = IntRange (minBalls, maxBalls);
		for (int i = 0; i < count; i++) {
			float xPos = Random.Range(bounds.minX, bounds.maxX);
			float zPos = Random.Range(bounds.minZ, bounds.maxZ);
			float size = Random.Range (minSize, maxSize);
			GameObject obj = Instantiate (prototype, new Vector3 (xPos, size/2.0f, zPos), Quaternion.identity) as GameObject; 
			obj.transform.localScale = new Vector3(size, size, size);
			Sphere sp = obj.GetComponent<Sphere>();
			sp.size = size;
			sp.bounds = bounds;
			this.balls.Add (sp);
		}
	}


	int IntRange(int min, int max) {
		return Mathf.RoundToInt (Random.Range (min, max));
	}

	void FixedUpdate() {
		foreach (Sphere sp in this.balls) {
			sp.ApplyForces(forces);
		}
	}

	// Update is called once per frame
	void Update () {
	
	}
}
